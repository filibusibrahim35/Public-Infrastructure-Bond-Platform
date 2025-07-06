(define-constant err-listing-not-found (err u200))
(define-constant err-listing-exists (err u201))
(define-constant err-invalid-price (err u202))
(define-constant err-cannot-buy-own-listing (err u203))
(define-constant err-listing-expired (err u204))
(define-constant err-insufficient-investment (err u205))
(define-constant err-invalid-amount (err u206))
(define-constant err-invalid-duration (err u207))
(define-constant err-no-investment (err u208))
(define-constant err-unauthorized (err u209))

(define-data-var next-listing-id uint u1)

(define-map marketplace-listings
  { listing-id: uint }
  {
    bond-id: uint,
    seller: principal,
    amount-for-sale: uint,
    price-per-unit: uint,
    total-price: uint,
    created-at: uint,
    expires-at: uint,
    is-active: bool
  }
)

(define-map bond-listings
  { bond-id: uint }
  { listing-ids: (list 100 uint) }
)

(define-map user-listings
  { user: principal }
  { listing-ids: (list 50 uint) }
)

(define-read-only (get-listing (listing-id uint))
  (map-get? marketplace-listings { listing-id: listing-id })
)

(define-read-only (get-bond-listings (bond-id uint))
  (default-to 
    { listing-ids: (list) }
    (map-get? bond-listings { bond-id: bond-id })
  )
)

(define-read-only (get-user-listings (user principal))
  (default-to 
    { listing-ids: (list) }
    (map-get? user-listings { user: user })
  )
)

(define-read-only (get-next-listing-id)
  (var-get next-listing-id)
)

(define-read-only (calculate-market-price (bond-id uint) (investment-amount uint))
  (match (contract-call? .PIB get-bond bond-id)
    bond-data 
      (let
        ((time-remaining (if (> (get end-block bond-data) stacks-block-height)
                           (- (get end-block bond-data) stacks-block-height)
                           u0))
         (total-duration (get duration-blocks bond-data))
         (yield-rate (get yield-rate bond-data)))
        (if (> time-remaining u0)
          (let
            ((remaining-yield (/ (* investment-amount (* yield-rate time-remaining)) u10000))
             (time-factor (/ (* time-remaining u100) total-duration)))
            (+ investment-amount (/ (* remaining-yield time-factor) u100)))
          investment-amount))
    u0
  )
)

(define-public (create-listing 
    (bond-id uint) 
    (amount-for-sale uint) 
    (price-per-unit uint)
    (duration-blocks uint))
  (let
    ((listing-id (var-get next-listing-id))
     (seller tx-sender)
     (total-price (* amount-for-sale price-per-unit))
     (current-bond-listings (get listing-ids (get-bond-listings bond-id)))
     (current-user-listings (get listing-ids (get-user-listings seller))))
    
    (asserts! (> amount-for-sale u0) err-invalid-amount)
    (asserts! (> price-per-unit u0) err-invalid-price)
    (asserts! (> duration-blocks u0) err-invalid-duration)
    
    (match (contract-call? .PIB get-investment bond-id seller)
      investment-data
        (begin
          (asserts! (>= (get amount investment-data) amount-for-sale) err-insufficient-investment)
          (asserts! (is-none (map-get? marketplace-listings { listing-id: listing-id })) err-listing-exists)
          
          (map-set marketplace-listings
            { listing-id: listing-id }
            {
              bond-id: bond-id,
              seller: seller,
              amount-for-sale: amount-for-sale,
              price-per-unit: price-per-unit,
              total-price: total-price,
              created-at: stacks-block-height,
              expires-at: (+ stacks-block-height duration-blocks),
              is-active: true
            }
          )
          
          (map-set bond-listings
            { bond-id: bond-id }
            { listing-ids: (unwrap! (as-max-len? (append current-bond-listings listing-id) u100) err-invalid-amount) }
          )
          
          (map-set user-listings
            { user: seller }
            { listing-ids: (unwrap! (as-max-len? (append current-user-listings listing-id) u50) err-invalid-amount) }
          )
          
          (var-set next-listing-id (+ listing-id u1))
          (ok listing-id))
      err-no-investment
    )
  )
)

(define-public (purchase-from-listing (listing-id uint) (amount-to-buy uint))
  (let
    ((listing (unwrap! (map-get? marketplace-listings { listing-id: listing-id }) err-listing-not-found))
     (buyer tx-sender)
     (seller (get seller listing))
     (bond-id (get bond-id listing))
     (purchase-price (* amount-to-buy (get price-per-unit listing))))
    
    (asserts! (get is-active listing) err-listing-not-found)
    (asserts! (<= stacks-block-height (get expires-at listing)) err-listing-expired)
    (asserts! (not (is-eq buyer seller)) err-cannot-buy-own-listing)
    (asserts! (<= amount-to-buy (get amount-for-sale listing)) err-insufficient-investment)
    (asserts! (> amount-to-buy u0) err-invalid-amount)
    
    (try! (stx-transfer? purchase-price buyer seller))
    
    (try! (contract-call? .PIB transfer-investment bond-id amount-to-buy buyer))
    
    (let
      ((remaining-amount (- (get amount-for-sale listing) amount-to-buy)))
      
      (if (is-eq remaining-amount u0)
        (map-set marketplace-listings
          { listing-id: listing-id }
          (merge listing { is-active: false })
        )
        (map-set marketplace-listings
          { listing-id: listing-id }
          (merge listing { 
            amount-for-sale: remaining-amount,
            total-price: (* remaining-amount (get price-per-unit listing))
          })
        )
      )
    )
    
    (ok purchase-price)
  )
)

(define-public (cancel-listing (listing-id uint))
  (let
    ((listing (unwrap! (map-get? marketplace-listings { listing-id: listing-id }) err-listing-not-found)))
    
    (asserts! (is-eq (get seller listing) tx-sender) err-unauthorized)
    (asserts! (get is-active listing) err-listing-not-found)
    
    (map-set marketplace-listings
      { listing-id: listing-id }
      (merge listing { is-active: false })
    )
    
    (ok true)
  )
)

(define-public (update-listing-price (listing-id uint) (new-price-per-unit uint))
  (let
    ((listing (unwrap! (map-get? marketplace-listings { listing-id: listing-id }) err-listing-not-found)))
    
    (asserts! (is-eq (get seller listing) tx-sender) err-unauthorized)
    (asserts! (get is-active listing) err-listing-not-found)
    (asserts! (> new-price-per-unit u0) err-invalid-price)
    
    (map-set marketplace-listings
      { listing-id: listing-id }
      (merge listing { 
        price-per-unit: new-price-per-unit,
        total-price: (* (get amount-for-sale listing) new-price-per-unit)
      })
    )
    
    (ok true)
  )
)

(define-read-only (get-market-summary (bond-id uint))
  (let
    ((bond-listings-data (get-bond-listings bond-id))
     (listing-ids (get listing-ids bond-listings-data)))
    {
      total-listings: (len listing-ids),
      bond-id: bond-id,
      has-active-listings: (> (len listing-ids) u0)
    }
  )
)

(define-read-only (is-listing-active (listing-id uint))
  (match (map-get? marketplace-listings { listing-id: listing-id })
    listing (and (get is-active listing) (<= stacks-block-height (get expires-at listing)))
    false
  )
)