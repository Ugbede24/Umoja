;; Cross-Chain Bridge Protocol - Stage 2
;; Enhanced bridge with merkle proof validation and cooling periods

;; Constants
(define-constant ERR-NOT-BRIDGE-OPERATOR (err u1))
(define-constant ERR-BRIDGE-NOT-ONLINE (err u2))
(define-constant ERR-INVALID-TRANSFER (err u3))
(define-constant ERR-ALREADY-PROCESSED (err u4))
(define-constant ERR-WRONG-MERKLE-PROOF (err u5))
(define-constant ERR-COOLING-PERIOD-ACTIVE (err u6))
(define-constant ERR-INSUFFICIENT-RESERVES (err u7))
(define-constant ERR-INVALID-PARAMETER (err u8))
(define-constant ERR-TRANSFER-EXISTS (err u9))
(define-constant MAX-TRANSFER-ID u100) ;; Maximum allowed transfer ID

;; Data Variables
(define-data-var bridge-operator principal tx-sender)
(define-data-var bridge-online bool false)
(define-data-var current-nonce uint u0)
(define-data-var current-timestamp uint u0) ;; Timestamp tracking for cooling periods
(define-data-var total-reserves uint u0)

;; Transfer Structure
(define-map bridge-transfers
    uint
    {
        description: (string-utf8 256),
        merkle-proof: (buff 32), ;; SHA256 hash of the expected merkle proof
        cooling-end: uint,       ;; Cooling period end timestamp
        amount: uint,
        processed: bool
    }
)

;; Transfer History
(define-map transfer-history
    uint
    {
        processed-at: (optional uint),
        processor: (optional principal)
    }
)

;; Authorization
(define-private (is-operator)
    (is-eq tx-sender (var-get bridge-operator)))

;; Timestamp Management
(define-public (update-timestamp (new-timestamp uint))
    (begin
        (asserts! (is-operator) ERR-NOT-BRIDGE-OPERATOR)
        ;; Validate timestamp is not in the past
        (asserts! (>= new-timestamp (var-get current-timestamp)) ERR-INVALID-PARAMETER)
        (var-set current-timestamp new-timestamp)
        (ok true)))

;; Bridge Management Functions
(define-public (activate-bridge)
    (begin
        (asserts! (is-operator) ERR-NOT-BRIDGE-OPERATOR)
        (var-set bridge-online true)
        (var-set current-nonce u0)
        (var-set total-reserves u0)
        (ok true)))

(define-public (deactivate-bridge)
    (begin
        (asserts! (is-operator) ERR-NOT-BRIDGE-OPERATOR)
        (var-set bridge-online false)
        (ok true)))

(define-public (register-transfer
    (transfer-id uint)
    (description (string-utf8 256))
    (merkle-proof (buff 32))
    (cooling-end uint)
    (amount uint))
    (begin
        (asserts! (is-operator) ERR-NOT-BRIDGE-OPERATOR)
        
        ;; Validate transfer-id is within acceptable range
        (asserts! (<= transfer-id MAX-TRANSFER-ID) ERR-INVALID-PARAMETER)
        
        ;; Check if transfer already exists to prevent overwriting
        (asserts! (is-none (map-get? bridge-transfers transfer-id)) ERR-TRANSFER-EXISTS)
        
        ;; Validate cooling end is in the future
        (asserts! (>= cooling-end (var-get current-timestamp)) ERR-INVALID-PARAMETER)
        
        ;; Validate merkle proof is not empty
        (asserts! (> (len merkle-proof) u0) ERR-INVALID-PARAMETER)
        
        ;; Validate description is not empty
        (asserts! (> (len description) u0) ERR-INVALID-PARAMETER)
        
        ;; Validate amount is positive
        (asserts! (> amount u0) ERR-INVALID-PARAMETER)
        
        ;; Set the transfer data
        (map-set bridge-transfers transfer-id
            {
                description: description,
                merkle-proof: merkle-proof,
                cooling-end: cooling-end,
                amount: amount,
                processed: false
            })
            
        ;; Calculate new reserves safely
        (let ((new-reserves (+ (var-get total-reserves) amount)))
            ;; Make sure the addition doesn't overflow
            (asserts! (>= new-reserves (var-get total-reserves)) ERR-INVALID-PARAMETER)
            ;; Update the total reserves
            (var-set total-reserves new-reserves))
        (ok true)))

;; Transfer Processing Functions
(define-public (process-transfer
    (transfer-id uint)
    (proof-data (buff 32)))
    (let (
        (transfer (unwrap! (map-get? bridge-transfers transfer-id) ERR-INVALID-TRANSFER))
        (current-time (var-get current-timestamp))
        )
        ;; Check transfer availability
        (asserts! (var-get bridge-online) ERR-BRIDGE-NOT-ONLINE)
        (asserts! (>= current-time (get cooling-end transfer)) ERR-COOLING-PERIOD-ACTIVE)
        (asserts! (not (get processed transfer)) ERR-ALREADY-PROCESSED)
        (asserts! (is-operator) ERR-NOT-BRIDGE-OPERATOR)
        
        ;; Verify merkle proof - directly compare the proofs
        (if (is-eq proof-data (get merkle-proof transfer))
            (begin
                ;; Update transfer status
                (map-set bridge-transfers transfer-id
                    (merge transfer {processed: true}))
                
                ;; Record history
                (map-set transfer-history transfer-id
                    {
                        processed-at: (some current-time),
                        processor: (some tx-sender)
                    })
                
                (ok true))
            ERR-WRONG-MERKLE-PROOF)))

;; Read-only functions
(define-read-only (get-transfer (transfer-id uint))
    (map-get? bridge-transfers transfer-id))

(define-read-only (get-transfer-history (transfer-id uint))
    (map-get? transfer-history transfer-id))

(define-read-only (get-current-timestamp)
    (var-get current-timestamp))

(define-read-only (get-bridge-stats)
    {
        online: (var-get bridge-online),
        current-nonce: (var-get current-nonce),
        total-reserves: (var-get total-reserves),
        current-timestamp: (var-get current-timestamp)
    })