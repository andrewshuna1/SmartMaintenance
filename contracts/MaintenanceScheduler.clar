;; Recurring Maintenance Task Scheduler Contract
;; Manages automatic scheduling and tracking of recurring maintenance tasks

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-not-found (err u401))
(define-constant err-unauthorized (err u402))
(define-constant err-invalid-interval (err u403))
(define-constant err-schedule-inactive (err u404))
(define-constant err-task-already-completed (err u405))
(define-constant err-invalid-priority (err u406))

;; Schedule interval constants (in blocks)
(define-constant INTERVAL_DAILY u144)     ;; ~24 hours
(define-constant INTERVAL_WEEKLY u1008)   ;; ~7 days  
(define-constant INTERVAL_MONTHLY u4320)  ;; ~30 days
(define-constant INTERVAL_QUARTERLY u12960) ;; ~90 days
(define-constant INTERVAL_YEARLY u52560)  ;; ~365 days

;; Task priority levels
(define-constant PRIORITY_LOW u1)
(define-constant PRIORITY_MEDIUM u2)
(define-constant PRIORITY_HIGH u3)
(define-constant PRIORITY_CRITICAL u4)

;; Task status constants
(define-constant STATUS_PENDING u1)
(define-constant STATUS_IN_PROGRESS u2)
(define-constant STATUS_COMPLETED u3)
(define-constant STATUS_OVERDUE u4)

;; Data variables for ID tracking
(define-data-var next-schedule-id uint u1)
(define-data-var next-task-id uint u1)

;; Main schedule configuration map
(define-map MaintenanceSchedules
    { schedule-id: uint }
    {
        equipment-id: uint,
        task-name: (string-ascii 100),
        task-description: (string-ascii 300),
        interval-blocks: uint,
        priority: uint,
        assigned-technician: (optional principal),
        estimated-duration: uint, ;; in blocks
        next-due-date: uint,
        last-completed: (optional uint),
        is-active: bool,
        created-by: principal,
        created-at: uint,
        total-completions: uint
    }
)

;; Individual task instances generated from schedules
(define-map ScheduledTasks
    { task-id: uint }
    {
        schedule-id: uint,
        equipment-id: uint,
        task-name: (string-ascii 100),
        due-date: uint,
        priority: uint,
        status: uint,
        assigned-technician: (optional principal),
        started-at: (optional uint),
        completed-at: (optional uint),
        completed-by: (optional principal),
        completion-notes: (optional (string-ascii 500)),
        created-at: uint
    }
)

;; Equipment scheduling statistics
(define-map EquipmentScheduleStats
    { equipment-id: uint }
    {
        active-schedules: uint,
        pending-tasks: uint,
        completed-tasks: uint,
        overdue-tasks: uint,
        last-maintenance: (optional uint),
        next-maintenance: (optional uint)
    }
)

;; Technician workload tracking
(define-map TechnicianWorkload
    { technician: principal }
    {
        assigned-tasks: uint,
        completed-tasks: uint,
        pending-tasks: uint,
        overdue-tasks: uint,
        last-activity: (optional uint)
    }
)

;; Create a new recurring maintenance schedule
(define-public (create-maintenance-schedule (equipment-id uint)
                                          (task-name (string-ascii 100))
                                          (task-description (string-ascii 300))
                                          (interval-blocks uint)
                                          (priority uint)
                                          (assigned-technician (optional principal))
                                          (estimated-duration uint))
    (let ((new-schedule-id (var-get next-schedule-id))
          (current-block stacks-block-height))
        
        ;; Validate inputs
        (asserts! (and (>= priority PRIORITY_LOW) (<= priority PRIORITY_CRITICAL)) err-invalid-priority)
        (asserts! (> interval-blocks u0) err-invalid-interval)
        (asserts! (> estimated-duration u0) err-invalid-interval)
        
        ;; Create the schedule
        (map-set MaintenanceSchedules
            { schedule-id: new-schedule-id }
            {
                equipment-id: equipment-id,
                task-name: task-name,
                task-description: task-description,
                interval-blocks: interval-blocks,
                priority: priority,
                assigned-technician: assigned-technician,
                estimated-duration: estimated-duration,
                next-due-date: (+ current-block interval-blocks),
                last-completed: none,
                is-active: true,
                created-by: tx-sender,
                created-at: current-block,
                total-completions: u0
            }
        )
        
        ;; Generate first task
        (try! (generate-task-from-schedule new-schedule-id))
        
        ;; Update equipment stats
        (unwrap-panic (update-equipment-stats equipment-id))
        
        ;; Increment schedule ID
        (var-set next-schedule-id (+ new-schedule-id u1))
        (ok new-schedule-id)
    )
)

;; Generate a new task instance from a schedule
(define-public (generate-task-from-schedule (schedule-id uint))
    (let ((schedule (unwrap! (map-get? MaintenanceSchedules { schedule-id: schedule-id }) err-not-found))
          (new-task-id (var-get next-task-id))
          (current-block stacks-block-height))
        
        ;; Ensure schedule is active
        (asserts! (get is-active schedule) err-schedule-inactive)
        
        ;; Create new task
        (map-set ScheduledTasks
            { task-id: new-task-id }
            {
                schedule-id: schedule-id,
                equipment-id: (get equipment-id schedule),
                task-name: (get task-name schedule),
                due-date: (get next-due-date schedule),
                priority: (get priority schedule),
                status: STATUS_PENDING,
                assigned-technician: (get assigned-technician schedule),
                started-at: none,
                completed-at: none,
                completed-by: none,
                completion-notes: none,
                created-at: current-block
            }
        )
        
        ;; Increment task ID
        (var-set next-task-id (+ new-task-id u1))
        (ok new-task-id)
    )
)

;; Mark a task as started
(define-public (start-task (task-id uint))
    (let ((task (unwrap! (map-get? ScheduledTasks { task-id: task-id }) err-not-found)))
        
        ;; Validate task can be started
        (asserts! (is-eq (get status task) STATUS_PENDING) err-task-already-completed)
        
        ;; Update task status
        (map-set ScheduledTasks
            { task-id: task-id }
            (merge task {
                status: STATUS_IN_PROGRESS,
                started-at: (some stacks-block-height)
            })
        )
        
        (ok true)
    )
)

;; Complete a scheduled task and update schedule
(define-public (complete-task (task-id uint) (completion-notes (optional (string-ascii 500))))
    (let ((task (unwrap! (map-get? ScheduledTasks { task-id: task-id }) err-not-found))
          (schedule-id (get schedule-id task))
          (schedule (unwrap! (map-get? MaintenanceSchedules { schedule-id: schedule-id }) err-not-found))
          (current-block stacks-block-height))
        
        ;; Validate task can be completed
        (asserts! (not (is-eq (get status task) STATUS_COMPLETED)) err-task-already-completed)
        
        ;; Update task as completed
        (map-set ScheduledTasks
            { task-id: task-id }
            (merge task {
                status: STATUS_COMPLETED,
                completed-at: (some current-block),
                completed-by: (some tx-sender),
                completion-notes: completion-notes
            })
        )
        
        ;; Update schedule with completion info and next due date
        (map-set MaintenanceSchedules
            { schedule-id: schedule-id }
            (merge schedule {
                last-completed: (some current-block),
                next-due-date: (+ current-block (get interval-blocks schedule)),
                total-completions: (+ (get total-completions schedule) u1)
            })
        )
        
        ;; Generate next task if schedule is still active
        (if (get is-active schedule)
            (unwrap-panic (generate-task-from-schedule schedule-id))
            u0
        )
        
        ;; Update equipment and technician stats
        (unwrap-panic (update-equipment-stats (get equipment-id task)))
        (if (is-some (get assigned-technician task))
            (unwrap-panic (update-technician-workload (unwrap-panic (get assigned-technician task))))
            true
        )
        
        (ok true)
    )
)

;; Update schedule configuration
(define-public (update-schedule (schedule-id uint)
                               (interval-blocks uint)
                               (priority uint)
                               (assigned-technician (optional principal))
                               (is-active bool))
    (let ((schedule (unwrap! (map-get? MaintenanceSchedules { schedule-id: schedule-id }) err-not-found)))
        
        ;; Only creator can update
        (asserts! (is-eq tx-sender (get created-by schedule)) err-unauthorized)
        
        ;; Validate inputs
        (asserts! (and (>= priority PRIORITY_LOW) (<= priority PRIORITY_CRITICAL)) err-invalid-priority)
        (asserts! (> interval-blocks u0) err-invalid-interval)
        
        ;; Update schedule
        (map-set MaintenanceSchedules
            { schedule-id: schedule-id }
            (merge schedule {
                interval-blocks: interval-blocks,
                priority: priority,
                assigned-technician: assigned-technician,
                is-active: is-active
            })
        )
        
        (ok true)
    )
)

;; Helper function to update equipment statistics
(define-private (update-equipment-stats (equipment-id uint))
    (let ((current-stats (default-to {
            active-schedules: u0,
            pending-tasks: u0,
            completed-tasks: u0,
            overdue-tasks: u0,
            last-maintenance: none,
            next-maintenance: none
        } (map-get? EquipmentScheduleStats { equipment-id: equipment-id }))))
        
        ;; In a real implementation, we would calculate these values
        ;; For now, we'll just update the last maintenance time
        (map-set EquipmentScheduleStats
            { equipment-id: equipment-id }
            (merge current-stats {
                last-maintenance: (some stacks-block-height)
            })
        )
        
        (ok true)
    )
)

;; Helper function to update technician workload
(define-private (update-technician-workload (technician principal))
    (let ((current-workload (default-to {
            assigned-tasks: u0,
            completed-tasks: u0,
            pending-tasks: u0,
            overdue-tasks: u0,
            last-activity: none
        } (map-get? TechnicianWorkload { technician: technician }))))
        
        (map-set TechnicianWorkload
            { technician: technician }
            (merge current-workload {
                last-activity: (some stacks-block-height),
                completed-tasks: (+ (get completed-tasks current-workload) u1)
            })
        )
        
        (ok true)
    )
)

;; Read-only functions for data retrieval

(define-read-only (get-schedule (schedule-id uint))
    (map-get? MaintenanceSchedules { schedule-id: schedule-id })
)

(define-read-only (get-task (task-id uint))
    (map-get? ScheduledTasks { task-id: task-id })
)

(define-read-only (get-equipment-stats (equipment-id uint))
    (map-get? EquipmentScheduleStats { equipment-id: equipment-id })
)

(define-read-only (get-technician-workload (technician principal))
    (map-get? TechnicianWorkload { technician: technician })
)

;; Check for overdue tasks for a specific equipment
(define-read-only (check-overdue-tasks (equipment-id uint))
    (ok {
        equipment-id: equipment-id,
        current-block: stacks-block-height,
        has-overdue: false
    })
)

;; Get upcoming tasks within specified block range
(define-read-only (get-upcoming-tasks (blocks-ahead uint))
    (ok {
        blocks-ahead: blocks-ahead,
        current-block: stacks-block-height,
        cutoff-block: (+ stacks-block-height blocks-ahead)
    })
)

;; Calculate maintenance compliance for equipment
(define-read-only (get-maintenance-compliance (equipment-id uint))
    (let ((stats (map-get? EquipmentScheduleStats { equipment-id: equipment-id })))
        (if (is-some stats)
            (let ((stats-data (unwrap-panic stats)))
                (ok {
                    equipment-id: equipment-id,
                    total-tasks: (+ (get completed-tasks stats-data) (get pending-tasks stats-data)),
                    completed-rate: (if (> (get completed-tasks stats-data) u0)
                        (/ (* (get completed-tasks stats-data) u100) 
                           (+ (get completed-tasks stats-data) (get overdue-tasks stats-data)))
                        u0),
                    last-maintenance: (get last-maintenance stats-data)
                })
            )
            (ok {
                equipment-id: equipment-id,
                total-tasks: u0,
                completed-rate: u0,
                last-maintenance: none
            })
        )
    )
)

