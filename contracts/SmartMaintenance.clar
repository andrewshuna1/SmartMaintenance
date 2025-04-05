;; SmartMaintenance Contract
;; Equipment maintenance tracking with service history, part verification and warranty management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant min-stake-amount u1000)

;; Data Variables
(define-data-var next-equipment-id uint u1)
(define-data-var next-service-id uint u1)

;; Define equipment status options
(define-constant STATUS_ACTIVE u1)
(define-constant STATUS_MAINTENANCE u2)
(define-constant STATUS_RETIRED u3)

;; Data Maps
(define-map Equipment
    { equipment-id: uint }
    {
        name: (string-ascii 50),
        manufacturer: (string-ascii 50),
        install-date: uint,
        warranty-end: uint,
        status: uint,
        owner: principal
    }
)

(define-map ServiceHistory
    { service-id: uint }
    {
        equipment-id: uint,
        service-date: uint,
        service-type: (string-ascii 50),
        provider: principal,
        parts-used: (string-ascii 100),
        notes: (string-ascii 200)
    }
)

(define-map ServiceProviders
    { provider: principal }
    {
        staked-amount: uint,
        is-active: bool,
        reputation-score: uint
    }
)

(define-map PartVerification
    { part-id: (string-ascii 50) }
    {
        is-genuine: bool,
        verified-by: principal,
        verify-date: uint
    }
)

;; Public Functions

;; Register new equipment
(define-public (register-equipment (name (string-ascii 50)) 
                                 (manufacturer (string-ascii 50)) 
                                 (warranty-duration uint))
    (let
        (
            (new-id (var-get next-equipment-id))
            (current-time stacks-block-height)
        )
        (map-set Equipment
            { equipment-id: new-id }
            {
                name: name,
                manufacturer: manufacturer,
                install-date: current-time,
                warranty-end: (+ current-time warranty-duration),
                status: STATUS_ACTIVE,
                owner: tx-sender
            }
        )
        (var-set next-equipment-id (+ new-id u1))
        (ok new-id)
    )
)

;; Stake tokens to become service provider
(define-public (stake-as-provider (amount uint))
    (let
        ((current-stake (default-to u0 (get staked-amount (map-get? ServiceProviders {provider: tx-sender}))))
        )
        (if (>= amount min-stake-amount)
            (begin
                (map-set ServiceProviders
                    {provider: tx-sender}
                    {
                        staked-amount: amount,
                        is-active: true,
                        reputation-score: u100
                    }
                )
                (ok true)
            )
            (err u104) ;; Insufficient stake
        )
    )
)

;; Record maintenance service
(define-public (record-service (equipment-id uint) 
                              (service-type (string-ascii 50))
                              (parts-used (string-ascii 100))
                              (notes (string-ascii 200)))
    (let
        (
            (new-service-id (var-get next-service-id))
            (provider-info (map-get? ServiceProviders {provider: tx-sender}))
        )
        (asserts! (is-some provider-info) err-unauthorized)
        (asserts! (get is-active (unwrap! provider-info err-unauthorized)) err-unauthorized)
        
        (map-set ServiceHistory
            {service-id: new-service-id}
            {
                equipment-id: equipment-id,
                service-date: stacks-block-height,
                service-type: service-type,
                provider: tx-sender,
                parts-used: parts-used,
                notes: notes
            }
        )
        (var-set next-service-id (+ new-service-id u1))
        (ok new-service-id)
    )
)

;; Verify parts
(define-public (verify-part (part-id (string-ascii 50)) (is-genuine bool))
    (let
        ((provider-info (map-get? ServiceProviders {provider: tx-sender})))
        (asserts! (is-some provider-info) err-unauthorized)
        (asserts! (get is-active (unwrap! provider-info err-unauthorized)) err-unauthorized)
        
        (map-set PartVerification
            {part-id: part-id}
            {
                is-genuine: is-genuine,
                verified-by: tx-sender,
                verify-date: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Read Only Functions

;; Get equipment details
(define-read-only (get-equipment (equipment-id uint))
    (map-get? Equipment {equipment-id: equipment-id})
)

;; Get service history
(define-read-only (get-service-record (service-id uint))
    (map-get? ServiceHistory {service-id: service-id})
)

;; Check warranty status
(define-read-only (check-warranty-status (equipment-id uint))
    (let
        ((equipment (map-get? Equipment {equipment-id: equipment-id})))
        (if (is-some equipment)
            (ok (> (get warranty-end (unwrap-panic equipment)) stacks-block-height))
            err-not-found
        )
    )
)

;; Get service provider info
(define-read-only (get-provider-info (provider principal))
    (map-get? ServiceProviders {provider: provider})
)

;; Get part verification status
(define-read-only (get-part-verification (part-id (string-ascii 50)))
    (map-get? PartVerification {part-id: part-id})
)

