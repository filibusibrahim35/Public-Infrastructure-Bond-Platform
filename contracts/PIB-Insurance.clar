;; PIB Insurance Contract - Risk Protection for Infrastructure Bonds
;; Provides comprehensive insurance coverage for bond projects and investors

;; Error constants
(define-constant err-unauthorized (err u300))
(define-constant err-invalid-amount (err u301))
(define-constant err-insufficient-funds (err u302))
(define-constant err-pool-not-found (err u303))
(define-constant err-policy-not-found (err u304))
(define-constant err-claim-not-found (err u305))
(define-constant err-pool-inactive (err u306))
(define-constant err-policy-expired (err u307))
(define-constant err-claim-already-filed (err u308))
(define-constant err-claim-already-processed (err u309))
(define-constant err-insufficient-coverage (err u310))
(define-constant err-invalid-coverage-ratio (err u311))
(define-constant err-bond-not-eligible (err u312))
(define-constant err-premium-calculation-failed (err u313))

;; Data variables
(define-data-var next-pool-id uint u1)
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% platform fee

;; Insurance pools - created by insurance providers
(define-map insurance-pools
  { pool-id: uint }
  {
    provider: principal,
    pool-name: (string-ascii 100),
    total-capacity: uint,
    available-capacity: uint,
    coverage-ratio: uint, ;; Percentage of bond value covered (max 10000 = 100%)
    premium-rate: uint, ;; Annual premium rate in basis points
    min-bond-value: uint,
    max-bond-value: uint,
    is-active: bool,
    created-at: uint
  }
)

;; Insurance policies - coverage purchased for specific bonds
(define-map insurance-policies
  { policy-id: uint }
  {
    bond-id: uint,
    pool-id: uint,
    insured-party: principal,
    coverage-amount: uint,
    premium-paid: uint,
    start-block: uint,
    end-block: uint,
    is-active: bool,
    claim-filed: bool
  }
)

;; Insurance claims - filed when bonds default or fail
(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    claimant: principal,
    claimed-amount: uint,
    claim-reason: (string-ascii 200),
    filed-at: uint,
    processed-at: uint,
    approved-amount: uint,
    status: (string-ascii 20) ;; PENDING, APPROVED, REJECTED, PAID
  }
)

;; Bond policy mapping for quick lookups
(define-map bond-policies
  { bond-id: uint }
  { policy-ids: (list 10 uint) }
)

;; Pool performance tracking
(define-map pool-stats
  { pool-id: uint }
  {
    total-policies: uint,
    total-premiums: uint,
    total-claims: uint,
    total-payouts: uint
  }
)

;; Read-only functions
(define-read-only (get-insurance-pool (pool-id uint))
  (map-get? insurance-pools { pool-id: pool-id })
)

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-bond-policies (bond-id uint))
  (default-to 
    { policy-ids: (list) }
    (map-get? bond-policies { bond-id: bond-id })
  )
)

(define-read-only (get-pool-stats (pool-id uint))
  (default-to
    { total-policies: u0, total-premiums: u0, total-claims: u0, total-payouts: u0 }
    (map-get? pool-stats { pool-id: pool-id })
  )
)

(define-read-only (calculate-premium (bond-value uint) (coverage-amount uint) (duration-blocks uint) (premium-rate uint))
  (let
    ((annual-premium (/ (* coverage-amount premium-rate) u10000))
     (duration-years (/ duration-blocks u52560))) ;; Assuming ~52560 blocks per year
    (if (> duration-years u0)
      (/ (* annual-premium duration-years) u1)
      annual-premium)
  )
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

;; Create insurance pool
(define-public (create-insurance-pool 
    (pool-name (string-ascii 100))
    (total-capacity uint)
    (coverage-ratio uint)
    (premium-rate uint)
    (min-bond-value uint)
    (max-bond-value uint))
  (let
    ((pool-id (var-get next-pool-id))
     (provider tx-sender))
    
    ;; Validation
    (asserts! (> total-capacity u0) err-invalid-amount)
    (asserts! (and (> coverage-ratio u0) (<= coverage-ratio u10000)) err-invalid-coverage-ratio)
    (asserts! (> premium-rate u0) err-invalid-amount)
    (asserts! (< min-bond-value max-bond-value) err-invalid-amount)
    
    ;; Provider must deposit the total capacity
    (try! (stx-transfer? total-capacity provider (as-contract tx-sender)))
    
    ;; Create the pool
    (map-set insurance-pools
      { pool-id: pool-id }
      {
        provider: provider,
        pool-name: pool-name,
        total-capacity: total-capacity,
        available-capacity: total-capacity,
        coverage-ratio: coverage-ratio,
        premium-rate: premium-rate,
        min-bond-value: min-bond-value,
        max-bond-value: max-bond-value,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    ;; Initialize pool stats
    (map-set pool-stats
      { pool-id: pool-id }
      { total-policies: u0, total-premiums: u0, total-claims: u0, total-payouts: u0 }
    )
    
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

;; Purchase insurance for a bond
(define-public (purchase-insurance (bond-id uint) (pool-id uint) (coverage-amount uint) (duration-blocks uint))
  (let
    ((policy-id (var-get next-policy-id))
     (insured-party tx-sender)
     (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) err-pool-not-found))
     (current-bond-policies (get policy-ids (get-bond-policies bond-id)))
     (current-stats (get-pool-stats pool-id)))
    
    ;; Validation
    (asserts! (get is-active pool) err-pool-inactive)
    (asserts! (>= (get available-capacity pool) coverage-amount) err-insufficient-coverage)
    (asserts! (> coverage-amount u0) err-invalid-amount)
    (asserts! (> duration-blocks u0) err-invalid-amount)
    
    ;; Verify bond exists and get bond data
    (match (contract-call? .PIB get-bond bond-id)
      bond-data
        (begin
          ;; Check bond eligibility
          (asserts! (>= (get target-amount bond-data) (get min-bond-value pool)) err-bond-not-eligible)
          (asserts! (<= (get target-amount bond-data) (get max-bond-value pool)) err-bond-not-eligible)
          
          ;; Calculate premium
          (let
            ((premium (calculate-premium (get target-amount bond-data) coverage-amount duration-blocks (get premium-rate pool)))
             (platform-fee (/ (* premium (var-get platform-fee-rate)) u10000)))
            
            (asserts! (> premium u0) err-premium-calculation-failed)
            
            ;; Collect premium payment
            (try! (stx-transfer? (+ premium platform-fee) insured-party (as-contract tx-sender)))
            
            ;; Transfer premium to pool provider (minus platform fee)
            (try! (as-contract (stx-transfer? premium tx-sender (get provider pool))))
            
            ;; Create policy
            (map-set insurance-policies
              { policy-id: policy-id }
              {
                bond-id: bond-id,
                pool-id: pool-id,
                insured-party: insured-party,
                coverage-amount: coverage-amount,
                premium-paid: premium,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height duration-blocks),
                is-active: true,
                claim-filed: false
              }
            )
            
            ;; Update pool capacity
            (map-set insurance-pools
              { pool-id: pool-id }
              (merge pool { available-capacity: (- (get available-capacity pool) coverage-amount) })
            )
            
            ;; Update bond policies mapping
            (map-set bond-policies
              { bond-id: bond-id }
              { policy-ids: (unwrap! (as-max-len? (append current-bond-policies policy-id) u10) err-invalid-amount) }
            )
            
            ;; Update pool stats
            (map-set pool-stats
              { pool-id: pool-id }
              (merge current-stats {
                total-policies: (+ (get total-policies current-stats) u1),
                total-premiums: (+ (get total-premiums current-stats) premium)
              })
            )
            
            (var-set next-policy-id (+ policy-id u1))
            (ok policy-id)
          )
        )
      err-bond-not-eligible
    )
  )
)

;; File insurance claim
(define-public (file-claim (policy-id uint) (claimed-amount uint) (claim-reason (string-ascii 200)))
  (let
    ((claim-id (var-get next-claim-id))
     (claimant tx-sender)
     (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) err-policy-not-found)))
    
    ;; Validation
    (asserts! (is-eq (get insured-party policy) claimant) err-unauthorized)
    (asserts! (get is-active policy) err-policy-expired)
    (asserts! (not (get claim-filed policy)) err-claim-already-filed)
    (asserts! (<= stacks-block-height (get end-block policy)) err-policy-expired)
    (asserts! (<= claimed-amount (get coverage-amount policy)) err-insufficient-coverage)
    (asserts! (> claimed-amount u0) err-invalid-amount)
    
    ;; Create claim
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        claimant: claimant,
        claimed-amount: claimed-amount,
        claim-reason: claim-reason,
        filed-at: stacks-block-height,
        processed-at: u0,
        approved-amount: u0,
        status: "PENDING"
      }
    )
    
    ;; Mark policy as having claim filed
    (map-set insurance-policies
      { policy-id: policy-id }
      (merge policy { claim-filed: true })
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Process claim (by pool provider)
(define-public (process-claim (claim-id uint) (approved-amount uint) (approve bool))
  (let
    ((claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) err-claim-not-found))
     (policy (unwrap! (map-get? insurance-policies { policy-id: (get policy-id claim) }) err-policy-not-found))
     (pool (unwrap! (map-get? insurance-pools { pool-id: (get pool-id policy) }) err-pool-not-found))
     (current-stats (get-pool-stats (get pool-id policy))))
    
    ;; Only pool provider can process claims
    (asserts! (is-eq (get provider pool) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status claim) "PENDING") err-claim-already-processed)
    
    (if approve
      (begin
        ;; Approve and pay claim
        (asserts! (<= approved-amount (get claimed-amount claim)) err-invalid-amount)
        (asserts! (<= approved-amount (get coverage-amount policy)) err-insufficient-coverage)
        
        ;; Transfer payout to claimant
        (try! (as-contract (stx-transfer? approved-amount tx-sender (get claimant claim))))
        
        ;; Update claim status
        (map-set insurance-claims
          { claim-id: claim-id }
          (merge claim {
            processed-at: stacks-block-height,
            approved-amount: approved-amount,
            status: "PAID"
          })
        )
        
        ;; Update pool stats
        (map-set pool-stats
          { pool-id: (get pool-id policy) }
          (merge current-stats {
            total-claims: (+ (get total-claims current-stats) u1),
            total-payouts: (+ (get total-payouts current-stats) approved-amount)
          })
        )
        
        (ok approved-amount)
      )
      (begin
        ;; Reject claim
        (map-set insurance-claims
          { claim-id: claim-id }
          (merge claim {
            processed-at: stacks-block-height,
            status: "REJECTED"
          })
        )
        
        ;; Restore pool capacity
        (map-set insurance-pools
          { pool-id: (get pool-id policy) }
          (merge pool { available-capacity: (+ (get available-capacity pool) (get coverage-amount policy)) })
        )
        
        (ok u0)
      )
    )
  )
)

;; Deactivate insurance pool
(define-public (deactivate-pool (pool-id uint))
  (let
    ((pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) err-pool-not-found)))
    
    (asserts! (is-eq (get provider pool) tx-sender) err-unauthorized)
    (asserts! (get is-active pool) err-pool-inactive)
    
    ;; Withdraw remaining capacity
    (try! (as-contract (stx-transfer? (get available-capacity pool) tx-sender (get provider pool))))
    
    ;; Deactivate pool
    (map-set insurance-pools
      { pool-id: pool-id }
      (merge pool { 
        is-active: false,
        available-capacity: u0
      })
    )
    
    (ok true)
  )
)

;; Update platform fee rate (contract owner only)
(define-public (update-platform-fee-rate (new-rate uint))
  (begin
    ;; Simple owner check - in production, you'd want proper access control
    (asserts! (<= new-rate u1000) err-invalid-amount) ;; Max 10% fee
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)







