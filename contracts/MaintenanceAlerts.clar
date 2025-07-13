(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-invalid-priority (err u203))
(define-constant err-already-acknowledged (err u204))

(define-constant ALERT_LOW u1)
(define-constant ALERT_MEDIUM u2)
(define-constant ALERT_HIGH u3)
(define-constant ALERT_CRITICAL u4)

(define-constant ALERT_MAINTENANCE_DUE u1)
(define-constant ALERT_WARRANTY_EXPIRING u2)
(define-constant ALERT_OVERDUE_SERVICE u3)
(define-constant ALERT_CUSTOM u4)

(define-data-var next-alert-id uint u1)
(define-data-var next-notification-id uint u1)

(define-map MaintenanceAlerts
    { alert-id: uint }
    {
        equipment-id: uint,
        alert-type: uint,
        priority: uint,
        title: (string-ascii 100),
        message: (string-ascii 300),
        created-at: uint,
        trigger-date: uint,
        is-active: bool,
        created-by: principal
    }
)

(define-map AlertNotifications
    { notification-id: uint }
    {
        alert-id: uint,
        recipient: principal,
        sent-at: uint,
        acknowledged: bool,
        acknowledged-at: (optional uint)
    }
)

(define-map UserAlertSettings
    { user: principal }
    {
        maintenance-alerts: bool,
        warranty-alerts: bool,
        overdue-alerts: bool,
        custom-alerts: bool,
        min-priority: uint
    }
)

(define-map EquipmentAlertConfig
    { equipment-id: uint }
    {
        maintenance-reminder-days: uint,
        warranty-reminder-days: uint,
        overdue-escalation-days: uint,
        alert-recipients: (list 5 principal)
    }
)

(define-public (create-maintenance-alert (equipment-id uint)
                                       (alert-type uint)
                                       (priority uint)
                                       (title (string-ascii 100))
                                       (message (string-ascii 300))
                                       (trigger-date uint))
    (let ((new-alert-id (var-get next-alert-id)))
        (asserts! (and (>= priority ALERT_LOW) (<= priority ALERT_CRITICAL)) err-invalid-priority)
        (asserts! (and (>= alert-type ALERT_MAINTENANCE_DUE) (<= alert-type ALERT_CUSTOM)) err-invalid-priority)
        (asserts! (> trigger-date stacks-block-height) err-invalid-priority)
        
        (map-set MaintenanceAlerts
            { alert-id: new-alert-id }
            {
                equipment-id: equipment-id,
                alert-type: alert-type,
                priority: priority,
                title: title,
                message: message,
                created-at: stacks-block-height,
                trigger-date: trigger-date,
                is-active: true,
                created-by: tx-sender
            }
        )
        
        (var-set next-alert-id (+ new-alert-id u1))
        (ok new-alert-id)
    )
)

(define-public (send-alert-notification (alert-id uint) (recipient principal))
    (let ((alert (map-get? MaintenanceAlerts { alert-id: alert-id }))
          (new-notification-id (var-get next-notification-id)))
        
        (asserts! (is-some alert) err-not-found)
        (asserts! (get is-active (unwrap-panic alert)) err-not-found)
        
        (map-set AlertNotifications
            { notification-id: new-notification-id }
            {
                alert-id: alert-id,
                recipient: recipient,
                sent-at: stacks-block-height,
                acknowledged: false,
                acknowledged-at: none
            }
        )
        
        (var-set next-notification-id (+ new-notification-id u1))
        (ok new-notification-id)
    )
)

(define-public (acknowledge-notification (notification-id uint))
    (let ((notification (map-get? AlertNotifications { notification-id: notification-id })))
        (asserts! (is-some notification) err-not-found)
        (asserts! (is-eq tx-sender (get recipient (unwrap-panic notification))) err-unauthorized)
        (asserts! (not (get acknowledged (unwrap-panic notification))) err-already-acknowledged)
        
        (map-set AlertNotifications
            { notification-id: notification-id }
            (merge (unwrap-panic notification) 
                   { acknowledged: true, acknowledged-at: (some stacks-block-height) })
        )
        
        (ok true)
    )
)

(define-public (set-user-alert-preferences (maintenance bool)
                                         (warranty bool)
                                         (overdue bool)
                                         (custom bool)
                                         (min-priority uint))
    (begin
        (asserts! (and (>= min-priority ALERT_LOW) (<= min-priority ALERT_CRITICAL)) err-invalid-priority)
        
        (map-set UserAlertSettings
            { user: tx-sender }
            {
                maintenance-alerts: maintenance,
                warranty-alerts: warranty,
                overdue-alerts: overdue,
                custom-alerts: custom,
                min-priority: min-priority
            }
        )
        
        (ok true)
    )
)

(define-public (configure-equipment-alerts (equipment-id uint)
                                         (maintenance-days uint)
                                         (warranty-days uint)
                                         (overdue-days uint)
                                         (recipients (list 5 principal)))
    (begin
        (map-set EquipmentAlertConfig
            { equipment-id: equipment-id }
            {
                maintenance-reminder-days: maintenance-days,
                warranty-reminder-days: warranty-days,
                overdue-escalation-days: overdue-days,
                alert-recipients: recipients
            }
        )
        
        (ok true)
    )
)

(define-public (deactivate-alert (alert-id uint))
    (let ((alert (map-get? MaintenanceAlerts { alert-id: alert-id })))
        (asserts! (is-some alert) err-not-found)
        (asserts! (is-eq tx-sender (get created-by (unwrap-panic alert))) err-unauthorized)
        
        (map-set MaintenanceAlerts
            { alert-id: alert-id }
            (merge (unwrap-panic alert) { is-active: false })
        )
        
        (ok true)
    )
)

(define-public (create-warranty-expiry-alert (equipment-id uint) (days-before uint))
    (let ((warranty-end-date (+ stacks-block-height (* days-before u144)))
          (alert-date (- warranty-end-date (* days-before u144))))
        
        (create-maintenance-alert 
            equipment-id
            ALERT_WARRANTY_EXPIRING
            ALERT_MEDIUM
            "Warranty Expiring Soon"
            "Equipment warranty will expire soon. Consider renewal or extended coverage."
            alert-date
        )
    )
)

(define-public (create-overdue-service-alert (equipment-id uint) (service-type (string-ascii 50)))
    (create-maintenance-alert
        equipment-id
        ALERT_OVERDUE_SERVICE
        ALERT_HIGH
        "Service Overdue"
        "Scheduled maintenance is overdue. Please schedule service immediately."
        stacks-block-height
    )
)

(define-read-only (get-alert (alert-id uint))
    (map-get? MaintenanceAlerts { alert-id: alert-id })
)

(define-read-only (get-notification (notification-id uint))
    (map-get? AlertNotifications { notification-id: notification-id })
)

(define-read-only (get-user-alert-settings (user principal))
    (map-get? UserAlertSettings { user: user })
)

(define-read-only (get-equipment-alert-config (equipment-id uint))
    (map-get? EquipmentAlertConfig { equipment-id: equipment-id })
)

(define-read-only (check-active-alerts-for-equipment (equipment-id uint))
    (let ((current-time stacks-block-height))
        (ok {
            has-active-alerts: true,
            check-time: current-time
        })
    )
)

(define-read-only (get-user-unacknowledged-notifications (user principal))
    (ok {
        user: user,
        check-time: stacks-block-height
    })
)

(define-public (bulk-acknowledge-notifications (notification-ids (list 10 uint)))
    (let ((results (map acknowledge-single-notification notification-ids)))
        (ok (len results))
    )
)

(define-private (acknowledge-single-notification (notification-id uint))
    (let ((notification (map-get? AlertNotifications { notification-id: notification-id })))
        (if (and (is-some notification)
                (is-eq tx-sender (get recipient (unwrap-panic notification)))
                (not (get acknowledged (unwrap-panic notification))))
            (begin
                (map-set AlertNotifications
                    { notification-id: notification-id }
                    (merge (unwrap-panic notification) 
                           { acknowledged: true, acknowledged-at: (some stacks-block-height) })
                )
                true
            )
            false
        )
    )
)

(define-read-only (get-alert-statistics (user principal))
    (ok {
        user: user,
        total-alerts: u0,
        acknowledged: u0,
        pending: u0,
        high-priority: u0
    })
)