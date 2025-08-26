;; FoodSecurity Network - Final (stacks-block-height)
(define-fungible-token food-credit)

(define-constant BLOCKS-PER-DAY u144)
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-invalid-food-item (err u103))
(define-constant err-insufficient-credits (err u104))
(define-constant err-food-not-available (err u105))

(define-data-var total-food-donations uint u0)
(define-data-var total-distributions uint u0)
(define-data-var next-food-id uint u1)

(define-map food-donations
  uint
  {
    donor: principal,
    food-type: (string-ascii 50),
    quantity: uint,
    expiry-block: uint,
    location: (string-ascii 100),
    status: (string-ascii 20),
    created-at: uint
  })

(define-map user-credits principal uint)
(define-map donation-history principal uint)
(define-map claim-history principal uint)

(define-public (donate-food-surplus
                (food-type (string-ascii 50))
                (quantity uint)
                (expiry-days uint)
                (location (string-ascii 100)))
  (let (
        (food-id (var-get next-food-id))
        (expiry-block (+ stacks-block-height (* expiry-days BLOCKS-PER-DAY)))
        (reward-credits (* quantity u10))
       )
    (begin
      (asserts! (> quantity u0) err-invalid-amount)
      (asserts! (> (len food-type) u0) err-invalid-food-item)
      (asserts! (> expiry-days u0) err-invalid-amount)
      (asserts! (> (len location) u0) err-invalid-food-item)

      (map-set food-donations food-id
        {
          donor: tx-sender,
          food-type: food-type,
          quantity: quantity,
          expiry-block: expiry-block,
          location: location,
          status: "available",
          created-at: stacks-block-height
        })

      (var-set next-food-id (+ food-id u1))
      (var-set total-food-donations (+ (var-get total-food-donations) u1))

      (map-set user-credits tx-sender
               (+ (default-to u0 (map-get? user-credits tx-sender)) reward-credits))

      (map-set donation-history tx-sender
               (+ (default-to u0 (map-get? donation-history tx-sender)) u1))

      (try! (ft-mint? food-credit reward-credits tx-sender))

      (print {
        event: "food-donated",
        food-id: food-id,
        donor: tx-sender,
        food-type: food-type,
        quantity: quantity,
        location: location,
        credits-earned: reward-credits
      })

      (ok food-id))))

(define-public (claim-food-distribution (food-id uint))
  (let (
        (food-info (unwrap! (map-get? food-donations food-id) err-food-not-available))
        (required-credits u50)
        (user-credits-balance (default-to u0 (map-get? user-credits tx-sender)))
       )
    (begin
      (asserts! (is-eq (get status food-info) "available") err-food-not-available)
      (asserts! (< stacks-block-height (get expiry-block food-info)) err-food-not-available)
      (asserts! (>= user-credits-balance required-credits) err-insufficient-credits)

      (map-set food-donations food-id
        (merge food-info { status: "claimed" }))

      (map-set user-credits tx-sender
               (- user-credits-balance required-credits))

      (try! (ft-burn? food-credit required-credits tx-sender))

      (var-set total-distributions (+ (var-get total-distributions) u1))

      (map-set claim-history tx-sender
               (+ (default-to u0 (map-get? claim-history tx-sender)) u1))

      (print {
        event: "food-claimed",
        food-id: food-id,
        claimer: tx-sender,
        food-type: (get food-type food-info),
        quantity: (get quantity food-info),
        location: (get location food-info),
        credits-used: required-credits
      })

      (ok true))))

(define-read-only (get-food-donation (food-id uint))
  (map-get? food-donations food-id))

(define-read-only (get-user-credits (user principal))
  (default-to u0 (map-get? user-credits user)))

(define-read-only (get-user-donation-count (user principal))
  (default-to u0 (map-get? donation-history user)))

(define-read-only (get-user-claim-count (user principal))
  (default-to u0 (map-get? claim-history user)))

(define-read-only (get-total-donations)
  (var-get total-food-donations))

(define-read-only (get-total-distributions)
  (var-get total-distributions))

(define-read-only (get-contract-stats)
  (ok {
    total-donations: (var-get total-food-donations),
    total-distributions: (var-get total-distributions),
    next-food-id: (var-get next-food-id)
  }))

(define-public (emergency-claim-food (food-id uint) (beneficiary principal))
  (let (
        (food-info (unwrap! (map-get? food-donations food-id) err-food-not-available))
       )
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (is-eq (get status food-info) "available") err-food-not-available)

      (map-set food-donations food-id
        (merge food-info { status: "claimed" }))

      (var-set total-distributions (+ (var-get total-distributions) u1))

      (print {
        event: "emergency-food-distribution",
        food-id: food-id,
        beneficiary: beneficiary,
        food-type: (get food-type food-info),
        quantity: (get quantity food-info)
      })

      (ok true))))