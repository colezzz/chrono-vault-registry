;; Chrono Vault: Historical Records Smart Contract


;; ==========================================
;; Administrator Configuration
;; ==========================================

;; System administrator - the deployer of this contract
(define-constant registry-supervisor tx-sender)

;; ==========================================
;; System Status Codes
;; ==========================================

(define-constant status-supervisor-only (err u300))
(define-constant status-record-not-found (err u301))
(define-constant status-duplicate-record (err u302))
(define-constant status-invalid-record-name (err u303))
(define-constant status-invalid-record-volume (err u304))
(define-constant status-entry-denied (err u305))
(define-constant status-forbidden-operation (err u306))
(define-constant status-viewing-restricted (err u307))
(define-constant status-invalid-category (err u308))

;; ==========================================
;; Data Storage Structures
;; ==========================================

;; Track total records in the system
(define-data-var registry-entries-count uint u0)

;; Primary storage for documentary records
(define-map registry-entries
  { entry-id: uint }
  {
    entry-name: (string-ascii 64),
    entry-custodian: principal,
    entry-volume: uint,
    entry-timestamp: uint,
    entry-summary: (string-ascii 128),
    entry-categories: (list 10 (string-ascii 32))
  }
)

;; Access control for documentary records
(define-map entry-access-controls
  { entry-id: uint, accessor: principal }
  { can-view: bool }
)

;; Validation registry for documentary records
(define-map entry-validations
  { entry-id: uint }
  {
    is-validated: bool,
    validated-by: principal,
    validation-timestamp: uint,
    validator-comments: (string-ascii 256)
  }
)

;; Registry of authorized validators
(define-map authorized-validators
  { validator: principal }
  { is-authorized: bool }
)

;; ==========================================
;; Internal Utility Functions
;; ==========================================

;; Retrieves the volume of a specific entry
(define-private (calculate-entry-volume (entry-id uint))
  (default-to u0
    (get entry-volume
      (map-get? registry-entries { entry-id: entry-id })
    )
  )
)

;; Validates a single category label
(define-private (validate-category-format (category (string-ascii 32)))
  (and
    (> (len category) u0)
    (< (len category) u33)
  )
)

;; Ensures all categories in a list are valid
(define-private (validate-category-list (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)
    (<= (len categories) u10)
    (is-eq (len (filter validate-category-format categories)) (len categories))
  )
)

;; Confirms if an entry exists in the registry
(define-private (entry-exists (entry-id uint))
  (is-some (map-get? registry-entries { entry-id: entry-id }))
)

;; Verifies if the specified user is the custodian of an entry
(define-private (is-entry-custodian (entry-id uint) (user principal))
  (match (map-get? registry-entries { entry-id: entry-id })
    entry-data (is-eq (get entry-custodian entry-data) user)
    false
  )
)

;; ==========================================
;; Public Registry Functions
;; ==========================================

;; Register a new documentary record with metadata
(define-public (register-new-entry (name (string-ascii 64)) (volume uint) (summary (string-ascii 128)) (categories (list 10 (string-ascii 32))))
  (let
    (
      (next-entry-id (+ (var-get registry-entries-count) u1))
    )
    ;; Input validation checks
    (asserts! (> (len name) u0) status-invalid-record-name)
    (asserts! (< (len name) u65) status-invalid-record-name)
    (asserts! (> volume u0) status-invalid-record-volume)
    (asserts! (< volume u1000000000) status-invalid-record-volume)
    (asserts! (> (len summary) u0) status-invalid-record-name)
    (asserts! (< (len summary) u129) status-invalid-record-name)
    (asserts! (validate-category-list categories) status-invalid-category)

    ;; Create the new record entry
    (map-insert registry-entries
      { entry-id: next-entry-id }
      {
        entry-name: name,
        entry-custodian: tx-sender,
        entry-volume: volume,
        entry-timestamp: block-height,
        entry-summary: summary,
        entry-categories: categories
      }
    )

    ;; Grant access to the creator
    (map-insert entry-access-controls
      { entry-id: next-entry-id, accessor: tx-sender }
      { can-view: true }
    )

    ;; Update the counter
    (var-set registry-entries-count next-entry-id)

    ;; Return the new ID
    (ok next-entry-id)
  )
)

;; Reassign custodianship of an entry to another user
(define-public (transfer-entry-custodianship (entry-id uint) (new-custodian principal))
  (let
    (
      (entry-data (unwrap! (map-get? registry-entries { entry-id: entry-id }) status-record-not-found))
    )
    ;; Verify entry exists and sender is authorized
    (asserts! (entry-exists entry-id) status-record-not-found)
    (asserts! (is-eq (get entry-custodian entry-data) tx-sender) status-forbidden-operation)

    ;; Update the custodian field
    (map-set registry-entries
      { entry-id: entry-id }
      (merge entry-data { entry-custodian: new-custodian })
    )

    (ok true)
  )
)

;; Modify entry details
(define-public (modify-entry-details (entry-id uint) (updated-name (string-ascii 64)) (updated-volume uint) (updated-summary (string-ascii 128)) (updated-categories (list 10 (string-ascii 32))))
  (let
    (
      (entry-data (unwrap! (map-get? registry-entries { entry-id: entry-id }) status-record-not-found))
    )
    ;; Validate entry existence and authorization
    (asserts! (entry-exists entry-id) status-record-not-found)
    (asserts! (is-eq (get entry-custodian entry-data) tx-sender) status-forbidden-operation)

    ;; Input validation
    (asserts! (> (len updated-name) u0) status-invalid-record-name)
    (asserts! (< (len updated-name) u65) status-invalid-record-name)
    (asserts! (> updated-volume u0) status-invalid-record-volume)
    (asserts! (< updated-volume u1000000000) status-invalid-record-volume)
    (asserts! (> (len updated-summary) u0) status-invalid-record-name)
    (asserts! (< (len updated-summary) u129) status-invalid-record-name)
    (asserts! (validate-category-list updated-categories) status-invalid-category)

    ;; Update the entry with new details
    (map-set registry-entries
      { entry-id: entry-id }
      (merge entry-data { 
        entry-name: updated-name, 
        entry-volume: updated-volume, 
        entry-summary: updated-summary, 
        entry-categories: updated-categories 
      })
    )

    (ok true)
  )
)

;; Remove an entry from the registry
(define-public (remove-registry-entry (entry-id uint))
  (let
    (
      (entry-data (unwrap! (map-get? registry-entries { entry-id: entry-id }) status-record-not-found))
    )
    ;; Verify entry exists and sender is authorized
    (asserts! (entry-exists entry-id) status-record-not-found)
    (asserts! (is-eq (get entry-custodian entry-data) tx-sender) status-forbidden-operation)

    ;; Delete the entry
    (map-delete registry-entries { entry-id: entry-id })

    (ok true)
  )
)

;; ==========================================
;; Access Control Functions
;; ==========================================

;; Remove access for a specific user
(define-public (revoke-entry-access (entry-id uint) (user principal))
  (let
    (
      (entry-data (unwrap! (map-get? registry-entries { entry-id: entry-id }) status-record-not-found))
    )
    ;; Validate entry exists and caller is the custodian
    (asserts! (entry-exists entry-id) status-record-not-found)
    (asserts! (is-eq (get entry-custodian entry-data) tx-sender) status-forbidden-operation)
    (asserts! (not (is-eq user tx-sender)) status-invalid-record-name) ;; Custodian cannot revoke their own access

    ;; Remove access permission
    (map-delete entry-access-controls { entry-id: entry-id, accessor: user })

    (ok true)
  )
)

;; Retrieve entry details if permitted
(define-public (retrieve-entry-details (entry-id uint))
  (let
    (
      (entry-data (unwrap! (map-get? registry-entries { entry-id: entry-id }) status-record-not-found))
      (access-record (map-get? entry-access-controls { entry-id: entry-id, accessor: tx-sender }))
    )
    ;; Confirm entry exists
    (asserts! (entry-exists entry-id) status-record-not-found)

    ;; Verify access authorization
    (asserts! (or 
                (is-eq (get entry-custodian entry-data) tx-sender)
                (is-some access-record)
                (and (is-some access-record) (get can-view (unwrap! access-record status-viewing-restricted)))
              ) 
              status-viewing-restricted)

    ;; Return the entry details
    (ok entry-data)
  )
)

;; ==========================================
;; Validation System Functions
;; ==========================================

;; Authenticate and validate an entry
(define-public (authenticate-registry-entry (entry-id uint) (validation-notes (string-ascii 256)))
  (let
    (
      (entry-data (unwrap! (map-get? registry-entries { entry-id: entry-id }) status-record-not-found))
      (validator-credentials (unwrap! (map-get? authorized-validators { validator: tx-sender }) status-forbidden-operation))
    )
    ;; Verify entry exists
    (asserts! (entry-exists entry-id) status-record-not-found)

    ;; Confirm validator authorization
    (asserts! (get is-authorized validator-credentials) status-forbidden-operation)

    (ok true)
  )
)

