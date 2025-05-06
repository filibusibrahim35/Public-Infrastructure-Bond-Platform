(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-bond-active (err u104))
(define-constant err-bond-not-active (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-bond-expired (err u107))
(define-constant err-bond-not-expired (err u108))
(define-constant err-bond-funded (err u109))
(define-constant err-bond-not-funded (err u110))
(define-constant err-invalid-amount (err u111))
(define-constant err-invalid-yield (err u112))
(define-constant err-invalid-duration (err u113))
(define-constant err-no-investment (err u114))

(define-data-var next-bond-id uint u1)

(define-map bonds
  { bond-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    target-amount: uint,
    current-amount: uint,
    yield-rate: uint,
    duration-blocks: uint,
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20),
    is-active: bool
  }
)

(define-map investments
  { bond-id: uint, investor: principal }
  {
    amount: uint,
    yield-claimed: bool
  }
)

(define-map bond-investors
  { bond-id: uint }
  { investors: (list 50 principal) }
)

(define-read-only (get-bond (bond-id uint))
  (map-get? bonds { bond-id: bond-id })
)

(define-read-only (get-investment (bond-id uint) (investor principal))
  (map-get? investments { bond-id: bond-id, investor: investor })
)

(define-read-only (get-bond-investors (bond-id uint))
  (match (map-get? bond-investors { bond-id: bond-id })
    investors investors
    { investors: (list) }
  )
)

(define-read-only (get-next-bond-id)
  (var-get next-bond-id)
)

(define-read-only (calculate-yield (amount uint) (yield-rate uint) (duration-blocks uint))
  (/ (* amount (* yield-rate duration-blocks)) u10000)
)

(define-public (create-bond 
    (name (string-ascii 100)) 
    (description (string-ascii 500)) 
    (target-amount uint) 
    (yield-rate uint) 
    (duration-blocks uint))
  (let
    ((bond-id (var-get next-bond-id)))
    
    (asserts! (> target-amount u0) err-invalid-amount)
    (asserts! (> yield-rate u0) err-invalid-yield)
    (asserts! (> duration-blocks u0) err-invalid-duration)
    
    (map-set bonds
      { bond-id: bond-id }
      {
        name: name,
        description: description,
        creator: tx-sender,
        target-amount: target-amount,
        current-amount: u0,
        yield-rate: yield-rate,
        duration-blocks: duration-blocks,
        start-block: u0,
        end-block: u0,
        status: "PENDING",
        is-active: false
      }
    )
    
    (map-set bond-investors
      { bond-id: bond-id }
      { investors: (list) }
    )
    
    (var-set next-bond-id (+ bond-id u1))
    (ok bond-id)
  )
)

(define-public (activate-bond (bond-id uint))
  (let
    ((bond (unwrap! (map-get? bonds { bond-id: bond-id }) err-not-found)))
    
    (asserts! (is-eq (get creator bond) tx-sender) err-unauthorized)
    (asserts! (is-eq (get is-active bond) false) err-bond-active)
    
    (map-set bonds
      { bond-id: bond-id }
      (merge bond {
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height (get duration-blocks bond)),
        status: "ACTIVE",
        is-active: true
      })
    )
    
    (ok true)
  )
)

(define-public (invest-in-bond (bond-id uint) (amount uint))
  (let
    ((bond (unwrap! (map-get? bonds { bond-id: bond-id }) err-not-found))
     (investor tx-sender)
     (current-investors (get investors (default-to { investors: (list) } (map-get? bond-investors { bond-id: bond-id }))))
     (current-investment (default-to { amount: u0, yield-claimed: false } (map-get? investments { bond-id: bond-id, investor: investor }))))
    
    (asserts! (get is-active bond) err-bond-not-active)
    (asserts! (<= stacks-block-height (get end-block bond)) err-bond-expired)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= (+ (get current-amount bond) amount) (get target-amount bond)) err-bond-funded)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set bonds
      { bond-id: bond-id }
      (merge bond {
        current-amount: (+ (get current-amount bond) amount)
      })
    )
    
    (map-set investments
      { bond-id: bond-id, investor: investor }
      {
        amount: (+ (get amount current-investment) amount),
        yield-claimed: false
      }
    )
    
    ;; (if (is-none (index-of current-investors investor))
    ;;   (map-set bond-investors
    ;;     { bond-id: bond-id }
    ;;     { investors: (append current-investors investor) }
    ;;   )
    ;;   true
    ;; )
    
    (ok true)
  )
)
(define-public (complete-bond (bond-id uint))
  (let
    ((bond (unwrap! (map-get? bonds { bond-id: bond-id }) err-not-found)))
    
    (asserts! (is-eq (get creator bond) tx-sender) err-unauthorized)
    (asserts! (get is-active bond) err-bond-not-active)
    (asserts! (>= stacks-block-height (get end-block bond)) err-bond-not-expired)
    
    (map-set bonds
      { bond-id: bond-id }
      (merge bond {
        status: "COMPLETED",
        is-active: false
      })
    )
    
    (ok true)
  )
)

(define-public (claim-yield (bond-id uint))
  (let
    ((bond (unwrap! (map-get? bonds { bond-id: bond-id }) err-not-found))
     (investment (unwrap! (map-get? investments { bond-id: bond-id, investor: tx-sender }) err-no-investment)))
    
    (asserts! (is-eq (get status bond) "COMPLETED") err-bond-active)
    (asserts! (not (get yield-claimed investment)) err-unauthorized)
    
    (let
      ((yield-amount (calculate-yield (get amount investment) (get yield-rate bond) (get duration-blocks bond))))
      
      (try! (as-contract (stx-transfer? (+ (get amount investment) yield-amount) tx-sender tx-sender)))
      
      (map-set investments
        { bond-id: bond-id, investor: tx-sender }
        (merge investment { yield-claimed: true })
      )
      
      (ok yield-amount)
    )
  )
)

(define-public (cancel-bond (bond-id uint))
  (let
    ((bond (unwrap! (map-get? bonds { bond-id: bond-id }) err-not-found)))
    
    (asserts! (is-eq (get creator bond) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status bond) "PENDING") err-bond-active)
    
    (map-set bonds
      { bond-id: bond-id }
      (merge bond {
        status: "CANCELLED",
        is-active: false
      })
    )
    
    (ok true)
  )
)

(define-public (refund-investors (bond-id uint))
  (let
    ((bond (unwrap! (map-get? bonds { bond-id: bond-id }) err-not-found))
     (investors-list (get investors (default-to { investors: (list) } (map-get? bond-investors { bond-id: bond-id })))))
    
    (asserts! (is-eq (get creator bond) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status bond) "CANCELLED") err-bond-not-active)
    
    (map refund-investor investors-list (list bond-id) )
    
    (ok true)
  )
)

(define-private (refund-investor (investor principal) (bond-id uint))
  (let
    ((investment (default-to { amount: u0, yield-claimed: false } (map-get? investments { bond-id: bond-id, investor: investor }))))
    
    (if (> (get amount investment) u0)
      (begin
        (try! (as-contract (stx-transfer? (get amount investment) investor tx-sender)))
        (map-set investments
          { bond-id: bond-id, investor: investor }
          (merge investment { amount: u0 })
        )
        (ok true)
      )
      (ok true)
    )
  )
)


(define-map creator-ratings
  { creator: principal }
  {
    bonds-created: uint,
    bonds-completed: uint,
    bonds-defaulted: uint,
    total-raised: uint
  }
)

(define-read-only (get-creator-rating (creator principal))
  (default-to 
    { bonds-created: u0, bonds-completed: u0, bonds-defaulted: u0, total-raised: u0 }
    (map-get? creator-ratings { creator: creator })
  )
)

(define-public (update-creator-stats (bond-id uint))
  (let 
    ((bond (unwrap! (map-get? bonds { bond-id: bond-id }) err-not-found))
     (creator (get creator bond))
     (current-stats (get-creator-rating creator)))
    
    (map-set creator-ratings
      { creator: creator }
      {
        bonds-created: (+ (get bonds-created current-stats) u1),
        bonds-completed: (get bonds-completed current-stats),
        bonds-defaulted: (get bonds-defaulted current-stats),
        total-raised: (+ (get total-raised current-stats) (get current-amount bond))
      }
    )
    (ok true)
  )
)


(define-constant err-transfer-failed (err u115))

(define-public (transfer-investment (bond-id uint) (amount uint) (recipient principal))
  (let
    ((bond (unwrap! (map-get? bonds { bond-id: bond-id }) err-not-found))
     (sender-investment (unwrap! (map-get? investments { bond-id: bond-id, investor: tx-sender }) err-no-investment))
     (recipient-investment (default-to { amount: u0, yield-claimed: false } 
       (map-get? investments { bond-id: bond-id, investor: recipient }))))
    
    (asserts! (>= (get amount sender-investment) amount) err-insufficient-funds)
    (asserts! (get is-active bond) err-bond-not-active)
    
    (map-set investments
      { bond-id: bond-id, investor: tx-sender }
      { 
        amount: (- (get amount sender-investment) amount),
        yield-claimed: (get yield-claimed sender-investment)
      }
    )
    
    (map-set investments
      { bond-id: bond-id, investor: recipient }
      {
        amount: (+ (get amount recipient-investment) amount),
        yield-claimed: (get yield-claimed recipient-investment)
      }
    )
    
    (ok true)
  )
)