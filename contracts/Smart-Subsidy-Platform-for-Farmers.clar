(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-invalid-status (err u106))
(define-constant err-milestone-not-ready (err u107))
(define-constant err-oracle-not-authorized (err u108))

(define-data-var next-farmer-id uint u1)
(define-data-var next-subsidy-id uint u1)
(define-data-var total-subsidies-distributed uint u0)

(define-map farmers
  { farmer-id: uint }
  {
    wallet: principal,
    name: (string-ascii 50),
    location: (string-ascii 100),
    farm-size: uint,
    verified: bool,
    registration-block: uint
  }
)

(define-map farmer-wallet-to-id
  { wallet: principal }
  { farmer-id: uint }
)

(define-map subsidies
  {
    subsidy-id: uint
  }
  {
    farmer-id: uint,
    total-amount: uint,
    disbursed-amount: uint,
    crop-type: (string-ascii 30),
    expected-yield: uint,
    actual-yield: uint,
    status: (string-ascii 20),
    created-block: uint,
    milestones-completed: uint,
    oracle-verified: bool
  }
)

(define-map milestones
  {
    subsidy-id: uint,
    milestone-id: uint
  }
  {
    description: (string-ascii 100),
    amount: uint,
    completed: bool,
    completion-block: uint,
    oracle-verified: bool
  }
)

(define-map authorized-oracles
  { oracle: principal }
  { authorized: bool }
)

(define-public (register-farmer (name (string-ascii 50)) (location (string-ascii 100)) (farm-size uint))
  (let
    (
      (farmer-id (var-get next-farmer-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-none (map-get? farmer-wallet-to-id { wallet: tx-sender })) err-already-exists)
    (asserts! (> farm-size u0) err-invalid-amount)
    
    (map-set farmers
      { farmer-id: farmer-id }
      {
        wallet: tx-sender,
        name: name,
        location: location,
        farm-size: farm-size,
        verified: false,
        registration-block: current-block
      }
    )
    
    (map-set farmer-wallet-to-id
      { wallet: tx-sender }
      { farmer-id: farmer-id }
    )
    
    (var-set next-farmer-id (+ farmer-id u1))
    (ok farmer-id)
  )
)

(define-public (verify-farmer (farmer-id uint))
  (let
    (
      (farmer (unwrap! (map-get? farmers { farmer-id: farmer-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set farmers
      { farmer-id: farmer-id }
      (merge farmer { verified: true })
    )
    (ok true)
  )
)

(define-public (create-subsidy (farmer-id uint) (total-amount uint) (crop-type (string-ascii 30)) (expected-yield uint))
  (let
    (
      (subsidy-id (var-get next-subsidy-id))
      (farmer (unwrap-panic (map-get? farmers { farmer-id: farmer-id })))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get verified farmer) err-unauthorized)
    (asserts! (> total-amount u0) err-invalid-amount)
    (asserts! (> expected-yield u0) err-invalid-amount)
    
    (map-set subsidies
      { subsidy-id: subsidy-id }
      {
        farmer-id: farmer-id,
        total-amount: total-amount,
        disbursed-amount: u0,
        crop-type: crop-type,
        expected-yield: expected-yield,
        actual-yield: u0,
        status: "active",
        created-block: current-block,
        milestones-completed: u0,
        oracle-verified: false
      }
    )
    
    (unwrap! (create-default-milestones subsidy-id total-amount) err-invalid-status)
    (var-set next-subsidy-id (+ subsidy-id u1))
    (ok subsidy-id)
  )
)

(define-private (create-default-milestones (subsidy-id uint) (total-amount uint))
  (let
    (
      (milestone-amount (/ total-amount u4))
    )
    (map-set milestones
      { subsidy-id: subsidy-id, milestone-id: u1 }
      {
        description: "Land preparation and seed purchase",
        amount: milestone-amount,
        completed: false,
        completion-block: u0,
        oracle-verified: false
      }
    )
    
    (map-set milestones
      { subsidy-id: subsidy-id, milestone-id: u2 }
      {
        description: "Planting and initial growth",
        amount: milestone-amount,
        completed: false,
        completion-block: u0,
        oracle-verified: false
      }
    )
    
    (map-set milestones
      { subsidy-id: subsidy-id, milestone-id: u3 }
      {
        description: "Mid-season maintenance",
        amount: milestone-amount,
        completed: false,
        completion-block: u0,
        oracle-verified: false
      }
    )
    
    (map-set milestones
      { subsidy-id: subsidy-id, milestone-id: u4 }
      {
        description: "Harvest completion",
        amount: (- total-amount (* milestone-amount u3)),
        completed: false,
        completion-block: u0,
        oracle-verified: false
      }
    )
    (ok true)
  )
)

(define-public (authorize-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-oracles { oracle: oracle } { authorized: true })
    (ok true)
  )
)

(define-public (verify-milestone (subsidy-id uint) (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { subsidy-id: subsidy-id, milestone-id: milestone-id }) err-not-found))
      (subsidy (unwrap! (map-get? subsidies { subsidy-id: subsidy-id }) err-not-found))
      (oracle-auth (default-to { authorized: false } (map-get? authorized-oracles { oracle: tx-sender })))
      (current-block stacks-block-height)
    )
    (asserts! (get authorized oracle-auth) err-oracle-not-authorized)
    (asserts! (not (get completed milestone)) err-invalid-status)
    
    (map-set milestones
      { subsidy-id: subsidy-id, milestone-id: milestone-id }
      (merge milestone {
        completed: true,
        completion-block: current-block,
        oracle-verified: true
      })
    )
    
    (map-set subsidies
      { subsidy-id: subsidy-id }
      (merge subsidy {
        milestones-completed: (+ (get milestones-completed subsidy) u1)
      })
    )
    (ok true)
  )
)

(define-public (disburse-milestone (subsidy-id uint) (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { subsidy-id: subsidy-id, milestone-id: milestone-id }) err-not-found))
      (subsidy (unwrap! (map-get? subsidies { subsidy-id: subsidy-id }) err-not-found))
      (farmer (unwrap! (map-get? farmers { farmer-id: (get farmer-id subsidy) }) err-not-found))
      (milestone-amount (get amount milestone))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get completed milestone) err-milestone-not-ready)
    (asserts! (get oracle-verified milestone) err-milestone-not-ready)
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) milestone-amount) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? milestone-amount tx-sender (get wallet farmer))))
    
    (map-set subsidies
      { subsidy-id: subsidy-id }
      (merge subsidy {
        disbursed-amount: (+ (get disbursed-amount subsidy) milestone-amount)
      })
    )
    
    (var-set total-subsidies-distributed (+ (var-get total-subsidies-distributed) milestone-amount))
    (ok milestone-amount)
  )
)

(define-public (report-harvest (subsidy-id uint) (actual-yield uint))
  (let
    (
      (subsidy (unwrap! (map-get? subsidies { subsidy-id: subsidy-id }) err-not-found))
      (farmer (unwrap! (map-get? farmers { farmer-id: (get farmer-id subsidy) }) err-not-found))
      (oracle-auth (default-to { authorized: false } (map-get? authorized-oracles { oracle: tx-sender })))
    )
    (asserts! (or (is-eq tx-sender (get wallet farmer)) (get authorized oracle-auth)) err-unauthorized)
    (asserts! (> actual-yield u0) err-invalid-amount)
    
    (map-set subsidies
      { subsidy-id: subsidy-id }
      (merge subsidy {
        actual-yield: actual-yield,
        oracle-verified: (get authorized oracle-auth),
        status: "completed"
      })
    )
    (ok true)
  )
)

(define-public (fund-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (stx-transfer? (stx-get-balance tx-sender) tx-sender (as-contract tx-sender))
  )
)

(define-read-only (get-farmer (farmer-id uint))
  (map-get? farmers { farmer-id: farmer-id })
)

(define-read-only (get-farmer-by-wallet (wallet principal))
  (match (map-get? farmer-wallet-to-id { wallet: wallet })
    farmer-data (map-get? farmers { farmer-id: (get farmer-id farmer-data) })
    none
  )
)

(define-read-only (get-subsidy (subsidy-id uint))
  (map-get? subsidies { subsidy-id: subsidy-id })
)

(define-read-only (get-milestone (subsidy-id uint) (milestone-id uint))
  (map-get? milestones { subsidy-id: subsidy-id, milestone-id: milestone-id })
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-read-only (get-total-subsidies-distributed)
  (var-get total-subsidies-distributed)
)

(define-read-only (is-oracle-authorized (oracle principal))
  (default-to false (get authorized (map-get? authorized-oracles { oracle: oracle })))
)

(define-read-only (get-subsidy-progress (subsidy-id uint))
  (match (map-get? subsidies { subsidy-id: subsidy-id })
    subsidy (ok {
      total-amount: (get total-amount subsidy),
      disbursed-amount: (get disbursed-amount subsidy),
      milestones-completed: (get milestones-completed subsidy),
      completion-percentage: (/ (* (get disbursed-amount subsidy) u100) (get total-amount subsidy))
    })
    err-not-found
  )
)
