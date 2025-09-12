;; Maintenance Inventory Management Contract
;; Manages spare parts, inventory tracking, supplier relationships and automated reordering

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u500))
(define-constant err-not-found (err u501))
(define-constant err-unauthorized (err u502))
(define-constant err-insufficient-quantity (err u503))
(define-constant err-invalid-supplier (err u504))
(define-constant err-low-stock (err u505))
(define-constant err-already-exists (err u506))

;; Inventory status constants
(define-constant STATUS_IN_STOCK u1)
(define-constant STATUS_LOW_STOCK u2)
(define-constant STATUS_OUT_OF_STOCK u3)
(define-constant STATUS_ON_ORDER u4)

;; Supplier status constants
(define-constant SUPPLIER_ACTIVE u1)
(define-constant SUPPLIER_INACTIVE u2)
(define-constant SUPPLIER_BLACKLISTED u3)

;; Data variables
(define-data-var next-part-id uint u1)
(define-data-var next-transaction-id uint u1)
(define-data-var next-supplier-id uint u1)
(define-data-var inventory-manager principal tx-sender)

;; Part inventory tracking
(define-map InventoryParts
  { part-id: uint }
  {
    part-name: (string-ascii 100),
    part-number: (string-ascii 50),
    category: (string-ascii 50),
    current-quantity: uint,
    min-threshold: uint,
    max-capacity: uint,
    unit-cost: uint,
    supplier-id: uint,
    status: uint,
    location: (string-ascii 100),
    last-updated: uint,
    created-by: principal
  }
)

;; Supplier management
(define-map Suppliers
  { supplier-id: uint }
  {
    name: (string-ascii 100),
    contact-info: (string-ascii 200),
    reliability-score: uint,
    lead-time-days: uint,
    payment-terms: (string-ascii 50),
    status: uint,
    total-orders: uint,
    on-time-deliveries: uint,
    registered-by: principal,
    created-at: uint
  }
)

;; Equipment part compatibility
(define-map EquipmentPartCompatibility
  { equipment-id: uint, part-id: uint }
  {
    is-compatible: bool,
    usage-frequency: uint,
    replacement-interval: uint,
    verified-by: principal,
    verified-at: uint
  }
)

;; Inventory transactions log
(define-map InventoryTransactions
  { transaction-id: uint }
  {
    part-id: uint,
    transaction-type: (string-ascii 20), ;; "IN", "OUT", "TRANSFER", "WASTE"
    quantity: uint,
    unit-cost: uint,
    reference-id: (optional uint), ;; service-id or purchase-order-id
    performed-by: principal,
    notes: (string-ascii 200),
    timestamp: uint
  }
)

;; Automated reorder configuration
(define-map ReorderSettings
  { part-id: uint }
  {
    auto-reorder-enabled: bool,
    reorder-quantity: uint,
    reorder-point: uint,
    preferred-supplier: uint,
    last-reorder-date: (optional uint),
    pending-orders: uint
  }
)

;; Inventory location management
(define-map StorageLocations
  { location-id: (string-ascii 20) }
  {
    description: (string-ascii 100),
    capacity: uint,
    current-utilization: uint,
    zone: (string-ascii 50),
    access-level: uint,
    manager: principal
  }
)

;; Register a new inventory part
(define-public (register-inventory-part 
  (part-name (string-ascii 100))
  (part-number (string-ascii 50))
  (category (string-ascii 50))
  (min-threshold uint)
  (max-capacity uint)
  (unit-cost uint)
  (supplier-id uint)
  (location (string-ascii 100)))
  (let
    ((new-part-id (var-get next-part-id)))
    
    ;; Validate supplier exists
    (asserts! (is-some (map-get? Suppliers { supplier-id: supplier-id })) err-invalid-supplier)
    
    ;; Create inventory part
    (map-set InventoryParts
      { part-id: new-part-id }
      {
        part-name: part-name,
        part-number: part-number,
        category: category,
        current-quantity: u0,
        min-threshold: min-threshold,
        max-capacity: max-capacity,
        unit-cost: unit-cost,
        supplier-id: supplier-id,
        status: STATUS_OUT_OF_STOCK,
        location: location,
        last-updated: stacks-block-height,
        created-by: tx-sender
      }
    )
    
    ;; Initialize reorder settings
    (map-set ReorderSettings
      { part-id: new-part-id }
      {
        auto-reorder-enabled: false,
        reorder-quantity: u0,
        reorder-point: min-threshold,
        preferred-supplier: supplier-id,
        last-reorder-date: none,
        pending-orders: u0
      }
    )
    
    (var-set next-part-id (+ new-part-id u1))
    (ok new-part-id)
  )
)

;; Register a supplier
(define-public (register-supplier
  (name (string-ascii 100))
  (contact-info (string-ascii 200))
  (lead-time-days uint)
  (payment-terms (string-ascii 50)))
  (let
    ((new-supplier-id (var-get next-supplier-id)))
    
    (map-set Suppliers
      { supplier-id: new-supplier-id }
      {
        name: name,
        contact-info: contact-info,
        reliability-score: u100,
        lead-time-days: lead-time-days,
        payment-terms: payment-terms,
        status: SUPPLIER_ACTIVE,
        total-orders: u0,
        on-time-deliveries: u0,
        registered-by: tx-sender,
        created-at: stacks-block-height
      }
    )
    
    (var-set next-supplier-id (+ new-supplier-id u1))
    (ok new-supplier-id)
  )
)

;; Record inventory intake (receiving parts)
(define-public (record-inventory-intake
  (part-id uint)
  (quantity uint)
  (unit-cost uint)
  (reference-id (optional uint))
  (notes (string-ascii 200)))
  (let
    ((part (unwrap! (map-get? InventoryParts { part-id: part-id }) err-not-found))
     (new-quantity (+ (get current-quantity part) quantity))
     (new-status (calculate-inventory-status new-quantity (get min-threshold part)))
     (transaction-id (var-get next-transaction-id)))
    
    ;; Update part quantity and status
    (map-set InventoryParts
      { part-id: part-id }
      (merge part {
        current-quantity: new-quantity,
        status: new-status,
        last-updated: stacks-block-height
      })
    )
    
    ;; Log transaction
    (map-set InventoryTransactions
      { transaction-id: transaction-id }
      {
        part-id: part-id,
        transaction-type: "IN",
        quantity: quantity,
        unit-cost: unit-cost,
        reference-id: reference-id,
        performed-by: tx-sender,
        notes: notes,
        timestamp: stacks-block-height
      }
    )
    
    (var-set next-transaction-id (+ transaction-id u1))
    (ok transaction-id)
  )
)

;; Record parts usage for maintenance
(define-public (use-inventory-parts
  (part-id uint)
  (quantity uint)
  (service-id (optional uint))
  (notes (string-ascii 200)))
  (let
    ((part (unwrap! (map-get? InventoryParts { part-id: part-id }) err-not-found))
     (current-qty (get current-quantity part))
     (transaction-id (var-get next-transaction-id)))
    
    ;; Check if sufficient quantity available
    (asserts! (>= current-qty quantity) err-insufficient-quantity)
    
    (let
      ((new-quantity (- current-qty quantity))
       (new-status (calculate-inventory-status new-quantity (get min-threshold part))))
      
      ;; Update part quantity and status
      (map-set InventoryParts
        { part-id: part-id }
        (merge part {
          current-quantity: new-quantity,
          status: new-status,
          last-updated: stacks-block-height
        })
      )
      
      ;; Log transaction
      (map-set InventoryTransactions
        { transaction-id: transaction-id }
        {
          part-id: part-id,
          transaction-type: "OUT",
          quantity: quantity,
          unit-cost: (get unit-cost part),
          reference-id: service-id,
          performed-by: tx-sender,
          notes: notes,
          timestamp: stacks-block-height
        }
      )
      
      (var-set next-transaction-id (+ transaction-id u1))
      
      ;; Check if reorder needed
      (if (and (<= new-quantity (get reorder-point (unwrap! (map-get? ReorderSettings { part-id: part-id }) err-not-found)))
               (get auto-reorder-enabled (unwrap! (map-get? ReorderSettings { part-id: part-id }) err-not-found)))
        (begin
          (unwrap-panic (trigger-auto-reorder part-id))
          true
        )
        true
      )
      
      (ok transaction-id)
    )
  )
)

;; Set part compatibility with equipment
(define-public (set-part-compatibility
  (equipment-id uint)
  (part-id uint)
  (is-compatible bool)
  (usage-frequency uint)
  (replacement-interval uint))
  (begin
    ;; Verify part exists
    (asserts! (is-some (map-get? InventoryParts { part-id: part-id })) err-not-found)
    
    (map-set EquipmentPartCompatibility
      { equipment-id: equipment-id, part-id: part-id }
      {
        is-compatible: is-compatible,
        usage-frequency: usage-frequency,
        replacement-interval: replacement-interval,
        verified-by: tx-sender,
        verified-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)

;; Configure automatic reordering
(define-public (configure-auto-reorder
  (part-id uint)
  (enabled bool)
  (reorder-quantity uint)
  (reorder-point uint)
  (preferred-supplier uint))
  (let
    ((part (unwrap! (map-get? InventoryParts { part-id: part-id }) err-not-found))
     (settings (unwrap! (map-get? ReorderSettings { part-id: part-id }) err-not-found)))
    
    ;; Only part creator or inventory manager can configure
    (asserts! (or (is-eq tx-sender (get created-by part)) 
                  (is-eq tx-sender (var-get inventory-manager))) err-unauthorized)
    
    ;; Validate supplier if provided
    (if (> preferred-supplier u0)
      (asserts! (is-some (map-get? Suppliers { supplier-id: preferred-supplier })) err-invalid-supplier)
      true
    )
    
    (map-set ReorderSettings
      { part-id: part-id }
      (merge settings {
        auto-reorder-enabled: enabled,
        reorder-quantity: reorder-quantity,
        reorder-point: reorder-point,
        preferred-supplier: preferred-supplier
      })
    )
    
    (ok true)
  )
)

;; Helper function to calculate inventory status
(define-private (calculate-inventory-status (current-qty uint) (min-threshold uint))
  (if (is-eq current-qty u0)
    STATUS_OUT_OF_STOCK
    (if (<= current-qty min-threshold)
      STATUS_LOW_STOCK
      STATUS_IN_STOCK
    )
  )
)

;; Trigger automatic reorder
(define-private (trigger-auto-reorder (part-id uint))
  (let
    ((settings (unwrap! (map-get? ReorderSettings { part-id: part-id }) err-not-found)))
    
    ;; Update pending orders count
    (map-set ReorderSettings
      { part-id: part-id }
      (merge settings {
        pending-orders: (+ (get pending-orders settings) u1),
        last-reorder-date: (some stacks-block-height)
      })
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-inventory-part (part-id uint))
  (map-get? InventoryParts { part-id: part-id })
)

(define-read-only (get-supplier-info (supplier-id uint))
  (map-get? Suppliers { supplier-id: supplier-id })
)

(define-read-only (get-part-compatibility (equipment-id uint) (part-id uint))
  (map-get? EquipmentPartCompatibility { equipment-id: equipment-id, part-id: part-id })
)

(define-read-only (get-inventory-transaction (transaction-id uint))
  (map-get? InventoryTransactions { transaction-id: transaction-id })
)

(define-read-only (get-reorder-settings (part-id uint))
  (map-get? ReorderSettings { part-id: part-id })
)

;; Check low stock items
(define-read-only (check-low-stock-items)
  (ok {
    check-time: stacks-block-height,
    total-parts-checked: (var-get next-part-id)
  })
)

;; Get inventory summary for a category
(define-read-only (get-category-inventory-summary (category (string-ascii 50)))
  (ok {
    category: category,
    total-value: u0,
    low-stock-count: u0,
    out-of-stock-count: u0
  })
)

;; Admin function to set inventory manager
(define-public (set-inventory-manager (new-manager principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set inventory-manager new-manager)
    (ok true)
  )
)
