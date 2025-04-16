;; Cross-Chain Bridge Protocol - Stage 3
;; A blockchain-based asset bridging system with sequential validation and relayer incentives

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
(define-data-var relayer-bond uint u1000000) ;; 1 STX
(define-data-var total-reserves uint u0)
(define-data-var current-timestamp uint u0) ;; Timestamp tracking for cooling periods

;; Transfer Structure
(define-map bridge-transfers
    uint
    {
        description: (string-utf8 256),
        merkle-proof: (buff 32), ;; SHA256 hash of the expected merkle proof
        cooling-end: uint,       ;; Cooling period end timestamp
        relayer-fee: uint,
        processed: bool
    }
)

;; Relayer Status Tracking
(define-map relayer-status
    principal
    {
        current-transfer: uint,
        processed-transfers: (list 20 uint),
        last-relay: uint,
        total-processed: uint
    }
)

;; Relay History
(define-map transfer-relays
    {transfer: uint, relayer: principal}
    {
        attempts: uint,
        relayed-at: (optional uint)
    }
)

;; Events
(define-map relay-events
    uint
    (list 10 {relayer: principal, relayed-at: uint})
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

(define-public (set-relayer-bond (new-bond uint))
    (begin
        (asserts! (is-operator) ERR-NOT-BRIDGE-OPERATOR)
        (asserts! (> new-bond u0) ERR-INVALID-PARAMETER)
        (var-set relayer-bond new-bond)
        (ok true)))

(define-public (register-transfer
    (transfer-id uint)
    (description (string-utf8 256))
    (merkle-proof (buff 32))
    (cooling-end uint)
    (relayer-fee uint))
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
        
        ;; Validate relayer fee is a positive amount
        (asserts! (> relayer-fee u0) ERR-INVALID-PARAMETER)
        
        ;; Set the transfer data
        (map-set bridge-transfers transfer-id
            {
                description: description,
                merkle-proof: merkle-proof,
                cooling-end: cooling-end,
                relayer-fee: relayer-fee,
                processed: false
            })
            
        ;; Calculate new reserves safely
        (let ((new-reserves (+ (var-get total-reserves) relayer-fee)))
            ;; Make sure the addition doesn't overflow
            (asserts! (>= new-reserves (var-get total-reserves)) ERR-INVALID-PARAMETER)
            ;; Update the total reserves
            (var-set total-reserves new-reserves))
        (ok true)))

;; Relayer Registration
(define-public (bond-relayer)
    (begin
        (asserts! (var-get bridge-online) ERR-BRIDGE-NOT-ONLINE)
        ;; Require relayer bond
        (try! (stx-transfer? (var-get relayer-bond) tx-sender (var-get bridge-operator)))
        
        (map-set relayer-status tx-sender
            {
                current-transfer: u0,
                processed-transfers: (list),
                last-relay: u0,
                total-processed: u0
            })
        (ok true)))

;; Transfer Processing Functions
(define-public (relay-transfer
    (transfer-id uint)
    (proof-data (buff 32)))
    (let (
        (transfer (unwrap! (map-get? bridge-transfers transfer-id) ERR-INVALID-TRANSFER))
        (relayer (unwrap! (map-get? relayer-status tx-sender) ERR-INVALID-TRANSFER))
        (current-time (var-get current-timestamp))
        )
        ;; Check transfer availability
        (asserts! (var-get bridge-online) ERR-BRIDGE-NOT-ONLINE)
        (asserts! (>= current-time (get cooling-end transfer)) ERR-COOLING-PERIOD-ACTIVE)
        (asserts! (not (get processed transfer)) ERR-ALREADY-PROCESSED)
        
        ;; Verify merkle proof - directly compare the proofs
        (if (is-eq proof-data (get merkle-proof transfer))
            (begin
                ;; Update transfer status
                (map-set bridge-transfers transfer-id
                    (merge transfer {processed: true}))
                
                ;; Update relayer status
                (map-set relayer-status tx-sender
                    (merge relayer {
                        current-transfer: (+ transfer-id u1),
                        processed-transfers: (unwrap! (as-max-len? 
                            (append (get processed-transfers relayer) transfer-id) u20)
                            ERR-INVALID-TRANSFER),
                        last-relay: current-time,
                        total-processed: (+ (get total-processed relayer) u1)
                    }))
                
                ;; Record relay
                (map-set transfer-relays
                    {transfer: transfer-id, relayer: tx-sender}
                    {
                        attempts: u1,
                        relayed-at: (some current-time)
                    })
                
                ;; Transfer relayer fee
                (try! (stx-transfer? (get relayer-fee transfer) (var-get bridge-operator) tx-sender))
                
                ;; Update total reserves
                (var-set total-reserves (- (var-get total-reserves) (get relayer-fee transfer)))
                
                ;; Record event
                (match (map-get? relay-events transfer-id)
                    events (map-set relay-events transfer-id
                        (unwrap! (as-max-len?
                            (append events {relayer: tx-sender, relayed-at: current-time})
                            u10)
                            ERR-INVALID-TRANSFER))
                    (map-set relay-events transfer-id
                        (list {relayer: tx-sender, relayed-at: current-time})))
                
                (ok true))
            ERR-WRONG-MERKLE-PROOF)))

;; Read-only functions
(define-read-only (get-transfer-description (transfer-id uint))
    (match (map-get? bridge-transfers transfer-id)
        transfer (if (>= (var-get current-timestamp) (get cooling-end transfer))
            (ok (get description transfer))
            ERR-COOLING-PERIOD-ACTIVE)
        ERR-INVALID-TRANSFER))

(define-read-only (get-transfer (transfer-id uint))
    (map-get? bridge-transfers transfer-id))

(define-read-only (get-relayer-status (relayer principal))
    (map-get? relayer-status relayer))

(define-read-only (get-relay-events (transfer-id uint))
    (map-get? relay-events transfer-id))

(define-read-only (get-transfer-relay (transfer-id uint) (relayer principal))
    (map-get? transfer-relays {transfer: transfer-id, relayer: relayer}))

(define-read-only (get-current-timestamp)
    (var-get current-timestamp))

(define-read-only (get-bridge-stats)
    {
        online: (var-get bridge-online),
        current-nonce: (var-get current-nonce),
        total-reserves: (var-get total-reserves),
        relayer-bond: (var-get relayer-bond),
        current-timestamp: (var-get current-timestamp)
    })