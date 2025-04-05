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

;; Transfer equipment ownership
(define-public (transfer-equipment-ownership (equipment-id uint) (new-owner principal))
    (let
        ((equipment (map-get? Equipment {equipment-id: equipment-id})))
        (asserts! (is-some equipment) err-not-found)
        (asserts! (is-eq (get owner (unwrap-panic equipment)) tx-sender) err-unauthorized)
        
        (map-set Equipment
            {equipment-id: equipment-id}
            (merge (unwrap-panic equipment) {owner: new-owner})
        )
        (ok true)
    )
)



;; Define maintenance schedule status
(define-constant SCHEDULE_PENDING u1)
(define-constant SCHEDULE_COMPLETED u2)
(define-constant SCHEDULE_CANCELLED u3)

;; Define next schedule ID
(define-data-var next-schedule-id uint u1)

;; Map for maintenance schedules
(define-map MaintenanceSchedule
    { schedule-id: uint }
    {
        equipment-id: uint,
        scheduled-date: uint,
        service-type: (string-ascii 50),
        status: uint,
        assigned-provider: (optional principal)
    }
)

;; Schedule maintenance
(define-public (schedule-maintenance (equipment-id uint) 
                                    (scheduled-date uint) 
                                    (service-type (string-ascii 50))
                                    (assigned-provider (optional principal)))
    (let
        ((equipment (map-get? Equipment {equipment-id: equipment-id}))
         (new-id (var-get next-schedule-id)))
        
        ;; Check if caller is equipment owner
        (asserts! (is-some equipment) err-not-found)
        (asserts! (is-eq (get owner (unwrap-panic equipment)) tx-sender) err-unauthorized)
        
        ;; Check if scheduled date is in the future
        (asserts! (> scheduled-date stacks-block-height) (err u106))
        
        ;; Create schedule
        (map-set MaintenanceSchedule
            {schedule-id: new-id}
            {
                equipment-id: equipment-id,
                scheduled-date: scheduled-date,
                service-type: service-type,
                status: SCHEDULE_PENDING,
                assigned-provider: assigned-provider
            }
        )
        
        (var-set next-schedule-id (+ new-id u1))
        (ok new-id)
    )
)

;; Complete scheduled maintenance
(define-public (complete-scheduled-maintenance (schedule-id uint))
    (let
        ((schedule (map-get? MaintenanceSchedule {schedule-id: schedule-id})))
        
        (asserts! (is-some schedule) err-not-found)
        (let
            ((unwrapped-schedule (unwrap-panic schedule))
             (assigned-provider (get assigned-provider unwrapped-schedule)))
            
            ;; Check if caller is the assigned provider
            (asserts! (or 
                (and (is-some assigned-provider) (is-eq (some tx-sender) assigned-provider))
                (is-none assigned-provider)) 
                err-unauthorized)
            
            ;; Update schedule status
            (map-set MaintenanceSchedule
                {schedule-id: schedule-id}
                (merge unwrapped-schedule {status: SCHEDULE_COMPLETED})
            )
            
            (ok true)
        )
    )
)

;; Get maintenance schedule
(define-read-only (get-maintenance-schedule (schedule-id uint))
    (map-get? MaintenanceSchedule {schedule-id: schedule-id})
)


;; Map for maintenance intervals
(define-map MaintenanceIntervals
    { equipment-id: uint, service-type: (string-ascii 50) }
    {
        interval-blocks: uint,
        last-service: uint,
        alert-threshold: uint
    }
)

;; Set maintenance interval
(define-public (set-maintenance-interval (equipment-id uint) 
                                        (service-type (string-ascii 50))
                                        (interval-blocks uint)
                                        (alert-threshold uint))
    (let
        ((equipment (map-get? Equipment {equipment-id: equipment-id})))
        
        ;; Check if caller is equipment owner
        (asserts! (is-some equipment) err-not-found)
        (asserts! (is-eq (get owner (unwrap-panic equipment)) tx-sender) err-unauthorized)
        
        ;; Set interval
        (map-set MaintenanceIntervals
            {equipment-id: equipment-id, service-type: service-type}
            {
                interval-blocks: interval-blocks,
                last-service: stacks-block-height,
                alert-threshold: alert-threshold
            }
        )
        
        (ok true)
    )
)

;; Update last service time (call this after service is performed)
(define-public (update-last-service-time (equipment-id uint) (service-type (string-ascii 50)))
    (let
        ((interval-data (map-get? MaintenanceIntervals 
                        {equipment-id: equipment-id, service-type: service-type})))
        
        (asserts! (is-some interval-data) err-not-found)
        
        ;; Update last service time
        (map-set MaintenanceIntervals
            {equipment-id: equipment-id, service-type: service-type}
            (merge (unwrap-panic interval-data) {last-service: stacks-block-height})
        )
        
        (ok true)
    )
)

;; Check if maintenance is due
(define-read-only (check-maintenance-due (equipment-id uint) (service-type (string-ascii 50)))
    (let
        ((interval-data (map-get? MaintenanceIntervals 
                        {equipment-id: equipment-id, service-type: service-type})))
        
        (if (is-some interval-data)
            (let
                ((unwrapped-data (unwrap-panic interval-data))
                 (last-service (get last-service unwrapped-data))
                 (interval (get interval-blocks unwrapped-data))
                 (threshold (get alert-threshold unwrapped-data))
                 (blocks-since-service (- stacks-block-height last-service))
                 (percent-to-next (/ (* blocks-since-service u100) interval)))
                
                (ok {
                    is-due: (>= blocks-since-service interval),
                    is-approaching: (>= percent-to-next threshold),
                    blocks-remaining: (if (>= blocks-since-service interval) 
                                         u0 
                                         (- interval blocks-since-service)),
                    percent-complete: percent-to-next
                })
            )
            err-not-found
        )
    )
)


;; Update equipment status
(define-public (update-equipment-status (equipment-id uint) (new-status uint))
    (let
        ((equipment (map-get? Equipment {equipment-id: equipment-id})))
        
        ;; Check if equipment exists and caller is owner
        (asserts! (is-some equipment) err-not-found)
        (asserts! (is-eq (get owner (unwrap-panic equipment)) tx-sender) err-unauthorized)
        
        ;; Validate status
        (asserts! (or (is-eq new-status STATUS_ACTIVE)
                     (is-eq new-status STATUS_MAINTENANCE)
                     (is-eq new-status STATUS_RETIRED))
                 err-invalid-status)
        
        ;; Update status
        (map-set Equipment
            {equipment-id: equipment-id}
            (merge (unwrap-panic equipment) {status: new-status})
        )
        
        (ok true)
    )
)

;; Map of authorized warranty extenders
(define-map WarrantyAuthorizers
    { manufacturer: (string-ascii 50), authorizer: principal }
    { is-authorized: bool }
)

;; Authorize warranty extender
(define-public (authorize-warranty-extender (manufacturer (string-ascii 50)) (authorizer principal))
    (begin
        ;; Only contract owner can authorize warranty extenders
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set WarrantyAuthorizers
            {manufacturer: manufacturer, authorizer: authorizer}
            {is-authorized: true}
        )
        
        (ok true)
    )
)

;; Extend equipment warranty
(define-public (extend-warranty (equipment-id uint) (extension-blocks uint))
    (let
        ((equipment (map-get? Equipment {equipment-id: equipment-id})))
        
        ;; Check if equipment exists
        (asserts! (is-some equipment) err-not-found)
        
        (let
            ((unwrapped-equipment (unwrap-panic equipment))
             (manufacturer (get manufacturer unwrapped-equipment))
             (is-authorized (default-to 
                            {is-authorized: false} 
                            (map-get? WarrantyAuthorizers 
                                     {manufacturer: manufacturer, authorizer: tx-sender}))))
            
            ;; Check if caller is authorized
            (asserts! (or 
                      (is-eq tx-sender contract-owner)
                      (get is-authorized is-authorized)
                      (is-eq tx-sender (get owner unwrapped-equipment)))
                     err-unauthorized)
            
            ;; Extend warranty
            (map-set Equipment
                {equipment-id: equipment-id}
                (merge unwrapped-equipment 
                      {warranty-end: (+ (get warranty-end unwrapped-equipment) extension-blocks)})
            )
            
            (ok true)
        )
    )
)


