(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-invalid-level (err u303))
(define-constant err-already-escalated (err u304))

(define-constant ESCALATION_LEVEL_1 u1)
(define-constant ESCALATION_LEVEL_2 u2)
(define-constant ESCALATION_LEVEL_3 u3)
(define-constant ESCALATION_LEVEL_FINAL u4)

(define-data-var next-escalation-rule-id uint u1)
(define-data-var next-escalation-log-id uint u1)

(define-map EscalationRules
    { rule-id: uint }
    {
        alert-type: uint,
        original-priority: uint,
        level-1-delay: uint,
        level-2-delay: uint,
        level-3-delay: uint,
        final-delay: uint,
        level-1-recipients: (list 3 principal),
        level-2-recipients: (list 3 principal),
        level-3-recipients: (list 3 principal),
        final-recipients: (list 3 principal),
        is-active: bool,
        created-by: principal
    }
)

(define-map AlertEscalations
    { alert-id: uint }
    {
        rule-id: uint,
        current-level: uint,
        last-escalation: uint,
        escalation-count: uint,
        is-completed: bool,
        created-at: uint
    }
)

(define-map EscalationLog
    { log-id: uint }
    {
        alert-id: uint,
        escalation-level: uint,
        escalated-at: uint,
        recipients-notified: (list 3 principal),
        escalated-by: principal
    }
)

(define-public (create-escalation-rule (alert-type uint)
                                      (original-priority uint)
                                      (level-1-delay uint)
                                      (level-2-delay uint)
                                      (level-3-delay uint)
                                      (final-delay uint)
                                      (level-1-recipients (list 3 principal))
                                      (level-2-recipients (list 3 principal))
                                      (level-3-recipients (list 3 principal))
                                      (final-recipients (list 3 principal)))
    (let ((new-rule-id (var-get next-escalation-rule-id)))
        (asserts! (and (>= alert-type u1) (<= alert-type u4)) err-invalid-level)
        (asserts! (and (>= original-priority u1) (<= original-priority u4)) err-invalid-level)
        (asserts! (> level-1-delay u0) err-invalid-level)
        (asserts! (> level-2-delay level-1-delay) err-invalid-level)
        (asserts! (> level-3-delay level-2-delay) err-invalid-level)
        (asserts! (> final-delay level-3-delay) err-invalid-level)
        
        (map-set EscalationRules
            { rule-id: new-rule-id }
            {
                alert-type: alert-type,
                original-priority: original-priority,
                level-1-delay: level-1-delay,
                level-2-delay: level-2-delay,
                level-3-delay: level-3-delay,
                final-delay: final-delay,
                level-1-recipients: level-1-recipients,
                level-2-recipients: level-2-recipients,
                level-3-recipients: level-3-recipients,
                final-recipients: final-recipients,
                is-active: true,
                created-by: tx-sender
            }
        )
        
        (var-set next-escalation-rule-id (+ new-rule-id u1))
        (ok new-rule-id)
    )
)

(define-public (register-alert-for-escalation (alert-id uint) (rule-id uint))
    (let ((rule (map-get? EscalationRules { rule-id: rule-id })))
        (asserts! (is-some rule) err-not-found)
        (asserts! (get is-active (unwrap-panic rule)) err-not-found)
        
        (map-set AlertEscalations
            { alert-id: alert-id }
            {
                rule-id: rule-id,
                current-level: u0,
                last-escalation: stacks-block-height,
                escalation-count: u0,
                is-completed: false,
                created-at: stacks-block-height
            }
        )
        
        (ok true)
    )
)

(define-public (escalate-alert (alert-id uint))
    (let ((escalation (map-get? AlertEscalations { alert-id: alert-id }))
          (rule-id (get rule-id (unwrap! escalation err-not-found)))
          (rule (map-get? EscalationRules { rule-id: rule-id }))
          (current-level (get current-level (unwrap-panic escalation)))
          (last-escalation (get last-escalation (unwrap-panic escalation)))
          (new-level (+ current-level u1)))
        
        (asserts! (is-some rule) err-not-found)
        (asserts! (not (get is-completed (unwrap-panic escalation))) err-already-escalated)
        (asserts! (<= new-level ESCALATION_LEVEL_FINAL) err-invalid-level)
        
        (let ((required-delay (get-required-delay rule new-level))
              (time-elapsed (- stacks-block-height last-escalation)))
            
            (asserts! (>= time-elapsed required-delay) err-unauthorized)
            
            (unwrap! (log-escalation alert-id new-level) err-not-found)
            
            (map-set AlertEscalations
                { alert-id: alert-id }
                (merge (unwrap-panic escalation)
                       {
                           current-level: new-level,
                           last-escalation: stacks-block-height,
                           escalation-count: (+ (get escalation-count (unwrap-panic escalation)) u1),
                           is-completed: (is-eq new-level ESCALATION_LEVEL_FINAL)
                       }
                )
            )
            
            (ok new-level)
        )
    )
)

(define-private (get-required-delay (rule (optional {alert-type: uint, original-priority: uint, level-1-delay: uint, level-2-delay: uint, level-3-delay: uint, final-delay: uint, level-1-recipients: (list 3 principal), level-2-recipients: (list 3 principal), level-3-recipients: (list 3 principal), final-recipients: (list 3 principal), is-active: bool, created-by: principal})) (level uint))
    (let ((rule-data (unwrap-panic rule)))
        (if (is-eq level ESCALATION_LEVEL_1)
            (get level-1-delay rule-data)
            (if (is-eq level ESCALATION_LEVEL_2)
                (get level-2-delay rule-data)
                (if (is-eq level ESCALATION_LEVEL_3)
                    (get level-3-delay rule-data)
                    (get final-delay rule-data)
                )
            )
        )
    )
)

(define-private (log-escalation (alert-id uint) (level uint))
    (let ((new-log-id (var-get next-escalation-log-id)))
        (map-set EscalationLog
            { log-id: new-log-id }
            {
                alert-id: alert-id,
                escalation-level: level,
                escalated-at: stacks-block-height,
                recipients-notified: (list),
                escalated-by: tx-sender
            }
        )
        
        (var-set next-escalation-log-id (+ new-log-id u1))
        (ok new-log-id)
    )
)

(define-public (stop-escalation (alert-id uint))
    (let ((escalation (map-get? AlertEscalations { alert-id: alert-id })))
        (asserts! (is-some escalation) err-not-found)
        
        (map-set AlertEscalations
            { alert-id: alert-id }
            (merge (unwrap-panic escalation) { is-completed: true })
        )
        
        (ok true)
    )
)

(define-public (update-escalation-rule (rule-id uint)
                                      (level-1-delay uint)
                                      (level-2-delay uint)
                                      (level-3-delay uint)
                                      (final-delay uint)
                                      (is-active bool))
    (let ((rule (map-get? EscalationRules { rule-id: rule-id })))
        (asserts! (is-some rule) err-not-found)
        (asserts! (is-eq tx-sender (get created-by (unwrap-panic rule))) err-unauthorized)
        
        (map-set EscalationRules
            { rule-id: rule-id }
            (merge (unwrap-panic rule)
                   {
                       level-1-delay: level-1-delay,
                       level-2-delay: level-2-delay,
                       level-3-delay: level-3-delay,
                       final-delay: final-delay,
                       is-active: is-active
                   }
            )
        )
        
        (ok true)
    )
)

(define-read-only (get-escalation-rule (rule-id uint))
    (map-get? EscalationRules { rule-id: rule-id })
)

(define-read-only (get-alert-escalation (alert-id uint))
    (map-get? AlertEscalations { alert-id: alert-id })
)

(define-read-only (get-escalation-log (log-id uint))
    (map-get? EscalationLog { log-id: log-id })
)

(define-read-only (check-escalation-due (alert-id uint))
    (let ((escalation (map-get? AlertEscalations { alert-id: alert-id })))
        (if (is-some escalation)
            (let ((escalation-data (unwrap-panic escalation))
                  (rule-id (get rule-id escalation-data))
                  (rule (map-get? EscalationRules { rule-id: rule-id }))
                  (current-level (get current-level escalation-data))
                  (last-escalation (get last-escalation escalation-data))
                  (next-level (+ current-level u1)))
                
                (if (and (is-some rule) (not (get is-completed escalation-data)) (<= next-level ESCALATION_LEVEL_FINAL))
                    (let ((required-delay (get-required-delay rule next-level))
                          (time-elapsed (- stacks-block-height last-escalation)))
                        (ok {
                            escalation-due: (>= time-elapsed required-delay),
                            next-level: next-level,
                            time-remaining: (if (>= time-elapsed required-delay) u0 (- required-delay time-elapsed))
                        })
                    )
                    (ok {
                        escalation-due: false,
                        next-level: u0,
                        time-remaining: u0
                    })
                )
            )
            err-not-found
        )
    )
)

(define-read-only (get-alerts-requiring-escalation (max-results uint))
    (ok {
        check-time: stacks-block-height,
        max-results: max-results
    })
)
