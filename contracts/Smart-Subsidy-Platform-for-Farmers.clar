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
(define-constant err-emergency-cooldown (err u109))
(define-constant err-emergency-limit-exceeded (err u110))
(define-constant err-emergency-already-claimed (err u111))
(define-constant emergency-cooldown-blocks u1008)
(define-constant max-emergency-percentage u30)

(define-constant max-coverage-percentage u80)
(define-constant base-premium-rate u5)
(define-constant weather-claim-window u144)

(define-data-var next-policy-id uint u1)
(define-data-var total-premiums-collected uint u0)
(define-data-var total-claims-paid uint u0)

(define-data-var emergency-fund-total uint u0)

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

(define-map emergency-requests
  { subsidy-id: uint }
  {
    farmer-id: uint,
    amount-requested: uint,
    reason: (string-ascii 100),
    request-block: uint,
    oracle-verified: bool,
    disbursed: bool,
    verification-block: uint
  }
)

(define-public (request-emergency-fund (subsidy-id uint) (amount-requested uint) (reason (string-ascii 100)))
  (let
    (
      (subsidy (unwrap! (map-get? subsidies { subsidy-id: subsidy-id }) err-not-found))
      (farmer (unwrap! (map-get? farmers { farmer-id: (get farmer-id subsidy) }) err-not-found))
      (max-emergency-amount (/ (* (get total-amount subsidy) max-emergency-percentage) u100))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get wallet farmer)) err-unauthorized)
    (asserts! (is-eq (get status subsidy) "active") err-invalid-status)
    (asserts! (is-none (map-get? emergency-requests { subsidy-id: subsidy-id })) err-emergency-already-claimed)
    (asserts! (<= amount-requested max-emergency-amount) err-emergency-limit-exceeded)
    (asserts! (> amount-requested u0) err-invalid-amount)
    
    (map-set emergency-requests
      { subsidy-id: subsidy-id }
      {
        farmer-id: (get farmer-id subsidy),
        amount-requested: amount-requested,
        reason: reason,
        request-block: current-block,
        oracle-verified: false,
        disbursed: false,
        verification-block: u0
      }
    )
    (ok true)
  )
)

(define-public (verify-emergency-claim (subsidy-id uint))
  (let
    (
      (emergency-request (unwrap! (map-get? emergency-requests { subsidy-id: subsidy-id }) err-not-found))
      (oracle-auth (default-to { authorized: false } (map-get? authorized-oracles { oracle: tx-sender })))
      (current-block stacks-block-height)
    )
    (asserts! (get authorized oracle-auth) err-oracle-not-authorized)
    (asserts! (not (get oracle-verified emergency-request)) err-invalid-status)
    
    (map-set emergency-requests
      { subsidy-id: subsidy-id }
      (merge emergency-request {
        oracle-verified: true,
        verification-block: current-block
      })
    )
    (ok true)
  )
)

(define-public (disburse-emergency-fund (subsidy-id uint))
  (let
    (
      (emergency-request (unwrap! (map-get? emergency-requests { subsidy-id: subsidy-id }) err-not-found))
      (subsidy (unwrap! (map-get? subsidies { subsidy-id: subsidy-id }) err-not-found))
      (farmer (unwrap! (map-get? farmers { farmer-id: (get farmer-id subsidy) }) err-not-found))
      (amount (get amount-requested emergency-request))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (get oracle-verified emergency-request) err-oracle-not-authorized)
    (asserts! (not (get disbursed emergency-request)) err-emergency-already-claimed)
    (asserts! (>= (- current-block (get verification-block emergency-request)) emergency-cooldown-blocks) err-emergency-cooldown)
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get wallet farmer))))
    
    (map-set emergency-requests
      { subsidy-id: subsidy-id }
      (merge emergency-request { disbursed: true })
    )
    
    (var-set emergency-fund-total (+ (var-get emergency-fund-total) amount))
    (ok amount)
  )
)

(define-read-only (get-emergency-request (subsidy-id uint))
  (map-get? emergency-requests { subsidy-id: subsidy-id })
)

(define-read-only (get-emergency-fund-total)
  (var-get emergency-fund-total)
)

(define-map farmer-scores
  { farmer-id: uint }
  {
    total-score: uint,
    completed-subsidies: uint,
    average-yield-ratio: uint,
    reliability-score: uint,
    last-updated: uint
  }
)


(define-public (calculate-farmer-score (farmer-id uint))
  (let
    (
      (farmer (unwrap! (map-get? farmers { farmer-id: farmer-id }) err-not-found))
      (current-score (default-to 
        { total-score: u500, completed-subsidies: u0, average-yield-ratio: u100, reliability-score: u500, last-updated: u0 }
        (map-get? farmer-scores { farmer-id: farmer-id })))
    )
    (asserts! (get verified farmer) err-unauthorized)
    
    (let
      (
        (subsidies-count (get-farmer-subsidies-count farmer-id))
        (avg-yield (get-farmer-average-yield-ratio farmer-id))
        (reliability (get-farmer-reliability-score farmer-id))
        (new-total-score (/ (+ (* avg-yield u4) (* reliability u6)) u10))
      )
      (map-set farmer-scores
        { farmer-id: farmer-id }
        {
          total-score: new-total-score,
          completed-subsidies: subsidies-count,
          average-yield-ratio: avg-yield,
          reliability-score: reliability,
          last-updated: stacks-block-height
        }
      )
      (ok new-total-score)
    )
  )
)

(define-private (get-farmer-subsidies-count (farmer-id uint))
  (get count (fold check-subsidy-completion 
    (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) 
    { farmer-id: farmer-id, count: u0 }))
)

(define-private (check-subsidy-completion (subsidy-id uint) (acc { farmer-id: uint, count: uint }))
  (match (map-get? subsidies { subsidy-id: subsidy-id })
    subsidy (if (and (is-eq (get farmer-id subsidy) (get farmer-id acc))
                     (is-eq (get status subsidy) "completed"))
                { farmer-id: (get farmer-id acc), count: (+ (get count acc) u1) }
                acc)
    acc
  )
)

(define-private (get-farmer-average-yield-ratio (farmer-id uint))
  (let
    (
      (yield-data (fold calculate-yield-ratio 
                    (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
                    { farmer-id: farmer-id, total-ratio: u0, count: u0 }))
    )
    (if (> (get count yield-data) u0)
        (/ (get total-ratio yield-data) (get count yield-data))
        u100)
  )
)

(define-private (calculate-yield-ratio (subsidy-id uint) (acc { farmer-id: uint, total-ratio: uint, count: uint }))
  (match (map-get? subsidies { subsidy-id: subsidy-id })
    subsidy (if (and (is-eq (get farmer-id subsidy) (get farmer-id acc))
                     (> (get actual-yield subsidy) u0))
                (let ((ratio (/ (* (get actual-yield subsidy) u100) (get expected-yield subsidy))))
                  { 
                    farmer-id: (get farmer-id acc), 
                    total-ratio: (+ (get total-ratio acc) ratio), 
                    count: (+ (get count acc) u1) 
                  })
                acc)
    acc
  )
)

(define-private (get-farmer-reliability-score (farmer-id uint))
  (let
    (
      (milestone-data (fold check-milestone-completion 
                       (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
                       { farmer-id: farmer-id, completed: u0, total: u0 }))
    )
    (if (> (get total milestone-data) u0)
        (/ (* (get completed milestone-data) u1000) (get total milestone-data))
        u500)
  )
)

(define-private (check-milestone-completion (subsidy-id uint) (acc { farmer-id: uint, completed: uint, total: uint }))
  (match (map-get? subsidies { subsidy-id: subsidy-id })
    subsidy (if (is-eq (get farmer-id subsidy) (get farmer-id acc))
                { 
                  farmer-id: (get farmer-id acc),
                  completed: (+ (get completed acc) (get milestones-completed subsidy)),
                  total: (+ (get total acc) u4)
                }
                acc)
    acc
  )
)

(define-read-only (get-farmer-score (farmer-id uint))
  (map-get? farmer-scores { farmer-id: farmer-id })
)

(define-read-only (get-farmer-rating (farmer-id uint))
  (match (map-get? farmer-scores { farmer-id: farmer-id })
    score (let ((total (get total-score score)))
            (if (>= total u800) "excellent"
            (if (>= total u600) "good"
            (if (>= total u400) "average"
            "needs-improvement"))))
    "unrated"
  )
)


(define-map insurance-policies
  { policy-id: uint }
  {
    farmer-id: uint,
    crop-type: (string-ascii 30),
    coverage-amount: uint,
    premium-paid: uint,
    coverage-start-block: uint,
    coverage-end-block: uint,
    active: bool
  }
)

(define-map weather-events
  { event-id: uint }
  {
    event-type: (string-ascii 20),
    severity: uint,
    location: (string-ascii 100),
    reported-block: uint,
    oracle-verified: bool
  }
)

(define-map insurance-claims
  { policy-id: uint }
  {
    claim-amount: uint,
    weather-event-id: uint,
    claim-block: uint,
    processed: bool,
    payout-amount: uint
  }
)

(define-public (purchase-insurance (farmer-id uint) (crop-type (string-ascii 30)) (coverage-amount uint) (coverage-blocks uint))
  (let
    (
      (farmer (unwrap! (map-get? farmers { farmer-id: farmer-id }) err-not-found))
      (policy-id (var-get next-policy-id))
      (premium (calculate-premium coverage-amount coverage-blocks))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get wallet farmer)) err-unauthorized)
    (asserts! (get verified farmer) err-unauthorized)
    (asserts! (> coverage-amount u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) premium) err-insufficient-funds)

    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))

    (map-set insurance-policies
      { policy-id: policy-id }
      {
        farmer-id: farmer-id,
        crop-type: crop-type,
        coverage-amount: coverage-amount,
        premium-paid: premium,
        coverage-start-block: current-block,
        coverage-end-block: (+ current-block coverage-blocks),
        active: true
      }
    )

    (var-set next-policy-id (+ policy-id u1))
    (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium))
    (ok policy-id)
  )
)

(define-private (calculate-premium (coverage-amount uint) (coverage-blocks uint))
  (/ (* (* coverage-amount base-premium-rate) coverage-blocks) u10000)
)

(define-public (file-insurance-claim (policy-id uint) (weather-event-id uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) err-not-found))
      (farmer (unwrap! (map-get? farmers { farmer-id: (get farmer-id policy) }) err-not-found))
      (weather-event (unwrap! (map-get? weather-events { event-id: weather-event-id }) err-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get wallet farmer)) err-unauthorized)
    (asserts! (get active policy) err-invalid-status)
    (asserts! (get oracle-verified weather-event) err-oracle-not-authorized)
    (asserts! (>= current-block (get coverage-start-block policy)) err-invalid-status)
    (asserts! (<= current-block (get coverage-end-block policy)) err-invalid-status)
    (asserts! (<= (- current-block (get reported-block weather-event)) weather-claim-window) err-invalid-status)
    (asserts! (is-none (map-get? insurance-claims { policy-id: policy-id })) err-already-exists)

    (let ((claim-amount (calculate-claim-amount (get coverage-amount policy) (get severity weather-event))))
      (map-set insurance-claims
        { policy-id: policy-id }
        {
          claim-amount: claim-amount,
          weather-event-id: weather-event-id,
          claim-block: current-block,
          processed: false,
          payout-amount: u0
        }
      )
      (ok claim-amount)
    )
  )
)

(define-private (calculate-claim-amount (coverage-amount uint) (severity uint))
  (/ (* coverage-amount (if (<= severity u100) severity u100)) u100)
)

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)

(define-read-only (get-insurance-stats)
  {
    total-premiums: (var-get total-premiums-collected),
    total-claims: (var-get total-claims-paid),
    active-policies: (var-get next-policy-id)
  }
)


(define-data-var next-resource-id uint u1)
(define-data-var next-request-id uint u1)

(define-map shared-resources
  { resource-id: uint }
  {
    owner-farmer-id: uint,
    resource-type: (string-ascii 30),
    description: (string-ascii 100),
    rental-cost: uint,
    available: bool,
    total-loans: uint
  }
)

(define-map resource-requests
  { request-id: uint }
  {
    requester-farmer-id: uint,
    resource-id: uint,
    request-block: uint,
    return-block: uint,
    status: (string-ascii 20),
    deposit-paid: uint
  }
)

(define-map lending-history
  { farmer-id: uint }
  { successful-loans: uint, failed-returns: uint, trust-score: uint }
)

(define-public (list-resource (resource-type (string-ascii 30)) (description (string-ascii 100)) (rental-cost uint))
  (let
    (
      (farmer-data (unwrap! (map-get? farmer-wallet-to-id { wallet: tx-sender }) err-unauthorized))
      (farmer-id (get farmer-id farmer-data))
      (farmer (unwrap! (map-get? farmers { farmer-id: farmer-id }) err-not-found))
      (resource-id (var-get next-resource-id))
    )
    (asserts! (get verified farmer) err-unauthorized)
    (asserts! (> rental-cost u0) err-invalid-amount)
    
    (map-set shared-resources
      { resource-id: resource-id }
      { owner-farmer-id: farmer-id, resource-type: resource-type, description: description,
        rental-cost: rental-cost, available: true, total-loans: u0 }
    )
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (request-resource (resource-id uint) (duration-blocks uint))
  (let
    (
      (resource (unwrap! (map-get? shared-resources { resource-id: resource-id }) err-not-found))
      (farmer-data (unwrap! (map-get? farmer-wallet-to-id { wallet: tx-sender }) err-unauthorized))
      (requester-id (get farmer-id farmer-data))
      (deposit (get rental-cost resource))
      (request-id (var-get next-request-id))
      (current-block stacks-block-height)
    )
    (asserts! (get available resource) err-invalid-status)
    (asserts! (not (is-eq requester-id (get owner-farmer-id resource))) err-unauthorized)
    (try! (stx-transfer? deposit tx-sender (as-contract tx-sender)))
    
    (map-set resource-requests
      { request-id: request-id }
      { requester-farmer-id: requester-id, resource-id: resource-id, request-block: current-block,
        return-block: (+ current-block duration-blocks), status: "active", deposit-paid: deposit }
    )
    (map-set shared-resources { resource-id: resource-id } (merge resource { available: false }))
    (var-set next-request-id (+ request-id u1))
    (ok request-id)
  )
)

(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b))

(define-public (return-resource (request-id uint))
  (let
    (
      (request (unwrap! (map-get? resource-requests { request-id: request-id }) err-not-found))
      (resource (unwrap! (map-get? shared-resources { resource-id: (get resource-id request) }) err-not-found))
      (owner (unwrap! (map-get? farmers { farmer-id: (get owner-farmer-id resource) }) err-not-found))
      (requester-history (default-to { successful-loans: u0, failed-returns: u0, trust-score: u1000 }
                          (map-get? lending-history { farmer-id: (get requester-farmer-id request) })))
    )
    (asserts! (is-eq (get status request) "active") err-invalid-status)
    
    (try! (as-contract (stx-transfer? (get deposit-paid request) tx-sender (get wallet owner))))
    (map-set resource-requests { request-id: request-id } (merge request { status: "completed" }))
    (map-set shared-resources { resource-id: (get resource-id request) }
      (merge resource { available: true, total-loans: (+ (get total-loans resource) u1) }))
    (map-set lending-history { farmer-id: (get requester-farmer-id request) }
      { successful-loans: (+ (get successful-loans requester-history) u1),
        failed-returns: (get failed-returns requester-history),
        trust-score: (min-uint u1000 (+ (get trust-score requester-history) u10)) })
    (ok true)
  )
)

(define-read-only (get-resource (resource-id uint))
  (map-get? shared-resources { resource-id: resource-id })
)

(define-read-only (get-farmer-trust-score (farmer-id uint))
  (default-to { successful-loans: u0, failed-returns: u0, trust-score: u1000 }
    (map-get? lending-history { farmer-id: farmer-id }))
)