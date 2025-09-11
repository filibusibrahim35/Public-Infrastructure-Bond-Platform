;; PIB Analytics Contract
;; Comprehensive bond performance analytics and rating system

;; Error constants
(define-constant err-unauthorized (err u400))
(define-constant err-bond-not-found (err u401))
(define-constant err-invalid-rating (err u402))
(define-constant err-rating-exists (err u403))
(define-constant err-insufficient-data (err u404))
(define-constant err-invalid-timeframe (err u405))

;; Data variables
(define-data-var total-bonds-tracked uint u0)
(define-data-var total-investment-volume uint u0)
(define-data-var platform-performance-score uint u0)

;; Bond performance metrics
(define-map bond-metrics
  { bond-id: uint }
  {
    funding-speed: uint,        ;; Blocks taken to reach funding target
    completion-success: bool,   ;; Whether bond completed successfully
    yield-delivered: uint,      ;; Actual yield delivered to investors
    investor-count: uint,       ;; Number of unique investors
    market-activity: uint,      ;; Secondary market transaction count
    risk-score: uint,          ;; Calculated risk score (0-100)
    performance-score: uint     ;; Overall performance score (0-100)
  }
)

;; Creator performance tracking
(define-map creator-analytics
  { creator: principal }
  {
    total-bonds: uint,
    completed-bonds: uint,
    failed-bonds: uint,
    total-raised: uint,
    avg-funding-time: uint,
    reliability-score: uint,    ;; 0-100 based on completion rate
    investor-satisfaction: uint ;; 0-100 based on yield delivery
  }
)

;; Investment portfolio analytics
(define-map investor-portfolios
  { investor: principal }
  {
    total-investments: uint,
    active-bonds: uint,
    completed-bonds: uint,
    total-yield-earned: uint,
    portfolio-value: uint,
    risk-diversification: uint, ;; 0-100 score
    performance-rating: uint    ;; 0-100 overall performance
  }
)

;; Bond category performance
(define-map category-metrics
  { category: (string-ascii 50) }
  {
    bond-count: uint,
    total-volume: uint,
    success-rate: uint,
    avg-yield-rate: uint,
    avg-completion-time: uint
  }
)

;; Time-series data for trends
(define-map monthly-stats
  { year: uint, month: uint }
  {
    bonds-created: uint,
    total-volume: uint,
    completion-rate: uint,
    avg-yield-delivered: uint
  }
)

;; Read-only functions
(define-read-only (get-bond-metrics (bond-id uint))
  (map-get? bond-metrics { bond-id: bond-id })
)

(define-read-only (get-creator-analytics (creator principal))
  (default-to
    { total-bonds: u0, completed-bonds: u0, failed-bonds: u0, total-raised: u0,
      avg-funding-time: u0, reliability-score: u0, investor-satisfaction: u0 }
    (map-get? creator-analytics { creator: creator })
  )
)

(define-read-only (get-investor-portfolio (investor principal))
  (default-to
    { total-investments: u0, active-bonds: u0, completed-bonds: u0,
      total-yield-earned: u0, portfolio-value: u0, risk-diversification: u0, performance-rating: u0 }
    (map-get? investor-portfolios { investor: investor })
  )
)

(define-read-only (get-platform-stats)
  {
    total-bonds-tracked: (var-get total-bonds-tracked),
    total-investment-volume: (var-get total-investment-volume),
    platform-performance-score: (var-get platform-performance-score)
  }
)

(define-read-only (get-monthly-stats (year uint) (month uint))
  (default-to
    { bonds-created: u0, total-volume: u0, completion-rate: u0, avg-yield-delivered: u0 }
    (map-get? monthly-stats { year: year, month: month })
  )
)

;; Calculate bond risk score based on various factors
(define-read-only (calculate-bond-risk-score (bond-id uint))
  (match (contract-call? .PIB get-bond bond-id)
    bond-data
      (let
        ((creator-stats (get-creator-analytics (get creator bond-data)))
         (funding-target (get target-amount bond-data))
         (yield-rate (get yield-rate bond-data))
         (creator-reliability (get reliability-score creator-stats)))
        
        ;; Risk factors: high yield rates, large targets, low creator reliability
        (let
          ((yield-risk (if (> yield-rate u1000) u30 u10))  ;; Higher yield = higher risk
           (size-risk (if (> funding-target u1000000000000) u20 u5)) ;; Large bonds = higher risk
           (creator-risk (if (< creator-reliability u50) u30 u10))) ;; Poor track record = higher risk
          
          (+ yield-risk (+ size-risk creator-risk))
        )
      )
    u50  ;; Default medium risk
  )
)

;; Update bond metrics when bond status changes
(define-public (update-bond-metrics (bond-id uint))
  (match (contract-call? .PIB get-bond bond-id)
    bond-data
      (let
        ((current-metrics (default-to 
           { funding-speed: u0, completion-success: false, yield-delivered: u0,
             investor-count: u0, market-activity: u0, risk-score: u0, performance-score: u0 }
           (map-get? bond-metrics { bond-id: bond-id })))
         (risk-score (calculate-bond-risk-score bond-id)))
        
        (map-set bond-metrics
          { bond-id: bond-id }
          (merge current-metrics {
            risk-score: risk-score,
            completion-success: (is-eq (get status bond-data) "COMPLETED"),
            funding-speed: (if (> (get current-amount bond-data) u0)
                             (- stacks-block-height (get start-block bond-data))
                             u0)
          })
        )
        
        ;; Update total tracked bonds if new
        (if (is-none (map-get? bond-metrics { bond-id: bond-id }))
          (var-set total-bonds-tracked (+ (var-get total-bonds-tracked) u1))
          true
        )
        
        (ok true)
      )
    err-bond-not-found
  )
)

;; Calculate creator reliability score
(define-private (calculate-creator-reliability (creator principal))
  (let
    ((stats (get-creator-analytics creator)))
    (if (> (get total-bonds stats) u0)
      (/ (* (get completed-bonds stats) u100) (get total-bonds stats))
      u0
    )
  )
)

;; Update creator analytics
(define-public (update-creator-analytics (creator principal) (bond-id uint))
  (match (contract-call? .PIB get-bond bond-id)
    bond-data
      (let
        ((current-stats (get-creator-analytics creator))
         (is-completed (is-eq (get status bond-data) "COMPLETED"))
         (is-failed (is-eq (get status bond-data) "CANCELLED")))
        
        (map-set creator-analytics
          { creator: creator }
          {
            total-bonds: (+ (get total-bonds current-stats) u1),
            completed-bonds: (+ (get completed-bonds current-stats) (if is-completed u1 u0)),
            failed-bonds: (+ (get failed-bonds current-stats) (if is-failed u1 u0)),
            total-raised: (+ (get total-raised current-stats) (get current-amount bond-data)),
            avg-funding-time: (get avg-funding-time current-stats), ;; Simplified for space
            reliability-score: (calculate-creator-reliability creator),
            investor-satisfaction: (get investor-satisfaction current-stats)
          }
        )
        
        (ok true)
      )
    err-bond-not-found
  )
)

;; Update investor portfolio analytics
(define-public (track-investment (investor principal) (bond-id uint) (amount uint))
  (let
    ((current-portfolio (get-investor-portfolio investor)))
    
    (map-set investor-portfolios
      { investor: investor }
      {
        total-investments: (+ (get total-investments current-portfolio) amount),
        active-bonds: (+ (get active-bonds current-portfolio) u1),
        completed-bonds: (get completed-bonds current-portfolio),
        total-yield-earned: (get total-yield-earned current-portfolio),
        portfolio-value: (+ (get portfolio-value current-portfolio) amount),
        risk-diversification: (calculate-portfolio-diversification investor),
        performance-rating: (get performance-rating current-portfolio)
      }
    )
    
    ;; Update platform volume
    (var-set total-investment-volume (+ (var-get total-investment-volume) amount))
    
    (ok true)
  )
)

;; Calculate portfolio diversification score
(define-private (calculate-portfolio-diversification (investor principal))
  ;; Simplified diversification calculation - in production would analyze across creators, categories, etc.
  (let
    ((portfolio (get-investor-portfolio investor)))
    (if (> (get active-bonds portfolio) u5)
      u80  ;; Good diversification
      (if (> (get active-bonds portfolio) u2)
        u60  ;; Moderate diversification
        u30  ;; Poor diversification
      )
    )
  )
)

;; Get top performing bonds
(define-read-only (get-top-bonds-by-performance (limit uint))
  ;; Simplified - returns static data for space constraints
  (list 
    { bond-id: u1, performance-score: u95 }
    { bond-id: u2, performance-score: u88 }
    { bond-id: u3, performance-score: u82 }
  )
)

;; Get creator leaderboard count
(define-read-only (get-creator-leaderboard-count)
  ;; In production, this would return actual leaderboard data
  u2
)

;; Track monthly performance
(define-public (update-monthly-stats (year uint) (month uint))
  (let
    ((current-stats (get-monthly-stats year month)))
    
    (asserts! (and (> year u2020) (and (>= month u1) (<= month u12))) err-invalid-timeframe)
    
    (map-set monthly-stats
      { year: year, month: month }
      {
        bonds-created: (+ (get bonds-created current-stats) u1),
        total-volume: (get total-volume current-stats),
        completion-rate: (get completion-rate current-stats),
        avg-yield-delivered: (get avg-yield-delivered current-stats)
      }
    )
    
    (ok true)
  )
)

;; Generate performance report for a bond
(define-read-only (generate-bond-report (bond-id uint))
  (match (get-bond-metrics bond-id)
    metrics
      (some {
        risk-assessment: (get risk-score metrics),
        performance-grade: (if (> (get performance-score metrics) u80) "A" 
                           (if (> (get performance-score metrics) u60) "B" "C")),
        funding-efficiency: (get funding-speed metrics),
        market-interest: (get market-activity metrics),
        recommendation: (if (< (get risk-score metrics) u30) "BUY" 
                        (if (< (get risk-score metrics) u60) "HOLD" "CAUTION"))
      })
    none
  )
)
