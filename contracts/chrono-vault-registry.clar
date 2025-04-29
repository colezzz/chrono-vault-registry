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
