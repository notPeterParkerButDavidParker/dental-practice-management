;; Dental Practice Management Smart Contract
;; Manages appointments, patient records, treatments, and insurance claims

;; Define error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-APPOINTMENT-NOT-FOUND (err u101))
(define-constant ERR-PATIENT-NOT-FOUND (err u102))
(define-constant ERR-TREATMENT-NOT-FOUND (err u103))
(define-constant ERR-INVALID-TIME-SLOT (err u104))
(define-constant ERR-APPOINTMENT-CONFLICT (err u105))
(define-constant ERR-INVALID-INPUT (err u106))
(define-constant ERR-CLAIM-NOT-FOUND (err u107))

;; Define the contract owner
(define-data-var contract-owner principal tx-sender)

;; Define counters for unique IDs
(define-data-var next-appointment-id uint u1)
(define-data-var next-patient-id uint u1)
(define-data-var next-treatment-id uint u1)
(define-data-var next-claim-id uint u1)

;; Define appointment status constants
(define-constant STATUS-SCHEDULED u1)
(define-constant STATUS-CONFIRMED u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-CANCELLED u4)

;; Patient data structure
(define-map patients
  { patient-id: uint }
  {
    name: (string-ascii 100),
    phone: (string-ascii 20),
    email: (string-ascii 100),
    date-of-birth: uint,
    insurance-provider: (string-ascii 50),
    insurance-id: (string-ascii 50),
    medical-history: (string-ascii 500),
    created-at: uint
  }
)

;; Appointment data structure
(define-map appointments
  { appointment-id: uint }
  {
    patient-id: uint,
    dentist: principal,
    appointment-date: uint,
    appointment-time: uint,
    duration-minutes: uint,
    treatment-type: (string-ascii 100),
    status: uint,
    notes: (string-ascii 500),
    created-at: uint
  }
)

;; Treatment plan data structure
(define-map treatment-plans
  { treatment-id: uint }
  {
    patient-id: uint,
    dentist: principal,
    treatment-name: (string-ascii 100),
    description: (string-ascii 500),
    estimated-cost: uint,
    sessions-required: uint,
    priority: uint,
    created-at: uint
  }
)

;; Insurance claims data structure
(define-map insurance-claims
  { claim-id: uint }
  {
    patient-id: uint,
    treatment-id: uint,
    claim-amount: uint,
    submitted-date: uint,
    status: (string-ascii 20),
    approval-code: (string-ascii 50),
    notes: (string-ascii 300)
  }
)

;; Patient lookup by principal
(define-map patient-principals
  { principal: principal }
  { patient-id: uint }
)

;; Dentist authorization
(define-map authorized-dentists
  { dentist: principal }
  { authorized: bool }
)

;; Time slot availability tracking
(define-map time-slots
  { date: uint, time: uint }
  { available: bool, dentist: principal }
)

;; Helper functions
(define-read-only (is-contract-owner)
  (is-eq tx-sender (var-get contract-owner))
)

(define-read-only (is-authorized-dentist (dentist principal))
  (default-to false (get authorized (map-get? authorized-dentists { dentist: dentist })))
)

(define-read-only (get-patient-id-by-principal (patient principal))
  (get patient-id (map-get? patient-principals { principal: patient }))
)

;; Administrative functions
(define-public (authorize-dentist (dentist principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-dentists { dentist: dentist } { authorized: true }))
  )
)

(define-public (revoke-dentist (dentist principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-dentists { dentist: dentist } { authorized: false }))
  )
)

;; Patient management functions
(define-public (register-patient 
  (name (string-ascii 100))
  (phone (string-ascii 20))
  (email (string-ascii 100))
  (date-of-birth uint)
  (insurance-provider (string-ascii 50))
  (insurance-id (string-ascii 50))
  (medical-history (string-ascii 500))
)
  (let 
    (
      (patient-id (var-get next-patient-id))
    )
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    (asserts! (> (len phone) u0) ERR-INVALID-INPUT)
    (map-set patients
      { patient-id: patient-id }
      {
        name: name,
        phone: phone,
        email: email,
        date-of-birth: date-of-birth,
        insurance-provider: insurance-provider,
        insurance-id: insurance-id,
        medical-history: medical-history,
        created-at: stacks-block-height
      }
    )
    (map-set patient-principals { principal: tx-sender } { patient-id: patient-id })
    (var-set next-patient-id (+ patient-id u1))
    (ok patient-id)
  )
)

(define-read-only (get-patient (patient-id uint))
  (map-get? patients { patient-id: patient-id })
)

(define-public (update-patient-info
  (patient-id uint)
  (phone (string-ascii 20))
  (email (string-ascii 100))
  (insurance-provider (string-ascii 50))
  (insurance-id (string-ascii 50))
)
  (let
    (
      (patient-data (unwrap! (map-get? patients { patient-id: patient-id }) ERR-PATIENT-NOT-FOUND))
      (caller-patient-id (get-patient-id-by-principal tx-sender))
    )
    (asserts! (or (is-authorized-dentist tx-sender) 
                  (is-some caller-patient-id)
                  (is-eq (some patient-id) caller-patient-id)) ERR-NOT-AUTHORIZED)
    (ok (map-set patients
      { patient-id: patient-id }
      (merge patient-data {
        phone: phone,
        email: email,
        insurance-provider: insurance-provider,
        insurance-id: insurance-id
      })
    ))
  )
)

;; Appointment management functions
(define-public (schedule-appointment
  (patient-id uint)
  (dentist principal)
  (appointment-date uint)
  (appointment-time uint)
  (duration-minutes uint)
  (treatment-type (string-ascii 100))
  (notes (string-ascii 500))
)
  (let
    (
      (appointment-id (var-get next-appointment-id))
    )
    (asserts! (is-some (map-get? patients { patient-id: patient-id })) ERR-PATIENT-NOT-FOUND)
    (asserts! (is-authorized-dentist dentist) ERR-NOT-AUTHORIZED)
    (asserts! (> duration-minutes u0) ERR-INVALID-INPUT)
    (asserts! (> appointment-date stacks-block-height) ERR-INVALID-TIME-SLOT)
    
    ;; Check if time slot is available
    (asserts! (default-to true (get available (map-get? time-slots { date: appointment-date, time: appointment-time }))) ERR-APPOINTMENT-CONFLICT)
    
    (map-set appointments
      { appointment-id: appointment-id }
      {
        patient-id: patient-id,
        dentist: dentist,
        appointment-date: appointment-date,
        appointment-time: appointment-time,
        duration-minutes: duration-minutes,
        treatment-type: treatment-type,
        status: STATUS-SCHEDULED,
        notes: notes,
        created-at: stacks-block-height
      }
    )
    
    ;; Mark time slot as unavailable
    (map-set time-slots 
      { date: appointment-date, time: appointment-time } 
      { available: false, dentist: dentist }
    )
    
    (var-set next-appointment-id (+ appointment-id u1))
    (ok appointment-id)
  )
)

(define-read-only (get-appointment (appointment-id uint))
  (map-get? appointments { appointment-id: appointment-id })
)

(define-public (confirm-appointment (appointment-id uint))
  (let
    (
      (appointment-data (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR-APPOINTMENT-NOT-FOUND))
    )
    (asserts! (or (is-eq tx-sender (get dentist appointment-data))
                  (is-eq (some (get patient-id appointment-data)) (get-patient-id-by-principal tx-sender))) ERR-NOT-AUTHORIZED)
    (ok (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment-data { status: STATUS-CONFIRMED })
    ))
  )
)

(define-public (complete-appointment (appointment-id uint))
  (let
    (
      (appointment-data (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR-APPOINTMENT-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get dentist appointment-data)) ERR-NOT-AUTHORIZED)
    (ok (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment-data { status: STATUS-COMPLETED })
    ))
  )
)

(define-public (cancel-appointment (appointment-id uint))
  (let
    (
      (appointment-data (unwrap! (map-get? appointments { appointment-id: appointment-id }) ERR-APPOINTMENT-NOT-FOUND))
    )
    (asserts! (or (is-eq tx-sender (get dentist appointment-data))
                  (is-eq (some (get patient-id appointment-data)) (get-patient-id-by-principal tx-sender))) ERR-NOT-AUTHORIZED)
    
    ;; Free up the time slot
    (map-set time-slots 
      { date: (get appointment-date appointment-data), time: (get appointment-time appointment-data) } 
      { available: true, dentist: (get dentist appointment-data) }
    )
    
    (ok (map-set appointments
      { appointment-id: appointment-id }
      (merge appointment-data { status: STATUS-CANCELLED })
    ))
  )
)

;; Treatment planning functions
(define-public (create-treatment-plan
  (patient-id uint)
  (treatment-name (string-ascii 100))
  (description (string-ascii 500))
  (estimated-cost uint)
  (sessions-required uint)
  (priority uint)
)
  (let
    (
      (treatment-id (var-get next-treatment-id))
    )
    (asserts! (is-authorized-dentist tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? patients { patient-id: patient-id })) ERR-PATIENT-NOT-FOUND)
    (asserts! (> (len treatment-name) u0) ERR-INVALID-INPUT)
    (asserts! (> sessions-required u0) ERR-INVALID-INPUT)
    
    (map-set treatment-plans
      { treatment-id: treatment-id }
      {
        patient-id: patient-id,
        dentist: tx-sender,
        treatment-name: treatment-name,
        description: description,
        estimated-cost: estimated-cost,
        sessions-required: sessions-required,
        priority: priority,
        created-at: stacks-block-height
      }
    )
    
    (var-set next-treatment-id (+ treatment-id u1))
    (ok treatment-id)
  )
)

(define-read-only (get-treatment-plan (treatment-id uint))
  (map-get? treatment-plans { treatment-id: treatment-id })
)

;; Insurance claim functions
(define-public (submit-insurance-claim
  (patient-id uint)
  (treatment-id uint)
  (claim-amount uint)
)
  (let
    (
      (claim-id (var-get next-claim-id))
    )
    (asserts! (is-authorized-dentist tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? patients { patient-id: patient-id })) ERR-PATIENT-NOT-FOUND)
    (asserts! (is-some (map-get? treatment-plans { treatment-id: treatment-id })) ERR-TREATMENT-NOT-FOUND)
    (asserts! (> claim-amount u0) ERR-INVALID-INPUT)
    
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        patient-id: patient-id,
        treatment-id: treatment-id,
        claim-amount: claim-amount,
        submitted-date: stacks-block-height,
        status: "SUBMITTED",
        approval-code: "",
        notes: ""
      }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-public (update-claim-status
  (claim-id uint)
  (status (string-ascii 20))
  (approval-code (string-ascii 50))
  (notes (string-ascii 300))
)
  (let
    (
      (claim-data (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND))
    )
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (ok (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim-data {
        status: status,
        approval-code: approval-code,
        notes: notes
      })
    ))
  )
)

;; Availability management
(define-public (set-time-slot-availability
  (date uint)
  (time uint)
  (available bool)
)
  (begin
    (asserts! (is-authorized-dentist tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set time-slots
      { date: date, time: time }
      { available: available, dentist: tx-sender }
    ))
  )
)

(define-read-only (check-time-slot-availability (date uint) (time uint))
  (get available (map-get? time-slots { date: date, time: time }))
)

;; Utility functions for patient education and data retrieval
(define-read-only (get-patient-appointments (patient-id uint))
  ;; This would typically return a list, but for simplicity we return the patient ID
  ;; In a full implementation, this would iterate through appointments
  (if (is-some (map-get? patients { patient-id: patient-id }))
    (ok patient-id)
    ERR-PATIENT-NOT-FOUND
  )
)

(define-read-only (get-dentist-schedule (dentist principal) (date uint))
  ;; Simplified - would return list of appointments for the dentist on given date
  (if (is-authorized-dentist dentist)
    (ok date)
    ERR-NOT-AUTHORIZED
  )
)

