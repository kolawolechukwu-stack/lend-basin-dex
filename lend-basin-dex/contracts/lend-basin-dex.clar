;; LendBasin Protocol - Clarity v2, Epoch 2.1
;; A collateralized debt position and yield basin protocol on Stacks
;;
;; Components:
;;   - Basin Manager: manages deposits, bToken minting, yield tracking
;;   - Derivatives Engine: synthetic asset minting via CDPs
;;   - Risk Oracle: dynamic price and risk scoring
;;   - Governance: BASIN token staking and fee distribution

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u102))
(define-constant ERR-BASIN-NOT-FOUND (err u103))
(define-constant ERR-POSITION-NOT-FOUND (err u104))
(define-constant ERR-UNDERCOLLATERALIZED (err u105))
(define-constant ERR-ALREADY-INITIALIZED (err u106))
(define-constant ERR-ORACLE-STALE (err u107))
(define-constant ERR-BELOW-MIN-COLLATERAL-RATIO (err u108))

;; Protocol parameters (basis points: 10000 = 100%)
(define-constant MIN-COLLATERAL-RATIO u15000)   ;; 150%
(define-constant LIQUIDATION-THRESHOLD u13000)  ;; 130%
(define-constant LIQUIDATION-PENALTY u1000)     ;; 10%
(define-constant PROTOCOL-FEE-RATE u50)         ;; 0.5%
(define-constant BASIN-FEE-SHARE u7000)         ;; 70% of fees go to basin participants
(define-constant STAKER-FEE-SHARE u2000)        ;; 20% of fees go to BASIN stakers
(define-constant TREASURY-FEE-SHARE u1000)      ;; 10% to treasury
(define-constant STABILITY-SCORE-DECAY u9900)   ;; 99% per epoch
(define-constant MAX-BASINS u20)
(define-constant PRICE-STALENESS-BLOCKS u144)   ;; ~24 hours at 10min blocks

;; ============================================================
;; FUNGIBLE TOKENS
;; ============================================================

;; bToken: represents proportional ownership of a basin
(define-fungible-token bToken)

;; Synthetic USD token minted against collateral
(define-fungible-token synth-usd)

;; BASIN governance token
(define-fungible-token basin-gov)

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var total-basins uint u0)
(define-data-var total-protocol-fees uint u0)
(define-data-var treasury-address principal CONTRACT-OWNER)
(define-data-var initialized bool false)
(define-data-var global-stability-index uint u10000) ;; starts at 1.0 (scaled 10000)

;; ============================================================
;; DATA MAPS
;; ============================================================

;; Basin: a yield-generating pool
(define-map basins
  { basin-id: uint }
  {
    name: (string-ascii 32),
    total-deposits: uint,       ;; total STX equivalent deposited
    total-bTokens: uint,        ;; total bTokens outstanding
    accumulated-yield: uint,    ;; yield earned since last settlement
    last-rebalance-block: uint,
    active: bool
  }
)

;; User deposit tracking per basin
(define-map basin-deposits
  { basin-id: uint, user: principal }
  {
    deposited-amount: uint,
    bTokens-held: uint,
    stability-score: uint,      ;; contribution score (scaled 10000)
    last-deposit-block: uint
  }
)

;; Collateralized Debt Positions (CDPs) for synthetic minting
(define-map cdp-positions
  { position-id: uint }
  {
    owner: principal,
    collateral-amount: uint,    ;; STX locked as collateral
    synth-minted: uint,         ;; synth-usd minted
    collateral-ratio: uint,     ;; current ratio (basis points)
    opened-at-block: uint,
    last-updated-block: uint
  }
)

;; Track CDPs per user
(define-map user-cdp-count
  { user: principal }
  { count: uint }
)

(define-data-var next-position-id uint u1)

;; Price oracle data
(define-map oracle-prices
  { asset: (string-ascii 12) }
  {
    price: uint,                ;; price in micro-USD (6 decimals)
    last-updated-block: uint,
    reporter: principal
  }
)

;; BASIN governance staking
(define-map gov-stakes
  { staker: principal }
  {
    staked-amount: uint,
    stake-start-block: uint,
    accumulated-rewards: uint
  }
)

;; Fee accumulator per basin
(define-map basin-fee-pool
  { basin-id: uint }
  { unclaimed-fees: uint }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Calculate bTokens to mint for a deposit
(define-private (calculate-btokens (basin-id uint) (deposit-amount uint))
  (match (map-get? basins { basin-id: basin-id })
    basin-data
      (if (is-eq (get total-bTokens basin-data) u0)
        deposit-amount  ;; 1:1 for first deposit
        (/ (* deposit-amount (get total-bTokens basin-data))
           (get total-deposits basin-data)))
    deposit-amount  ;; fallback 1:1
  )
)

;; Calculate current collateral ratio (basis points)
(define-private (get-collateral-ratio (collateral-stx uint) (synth-debt uint))
  (let (
    (stx-price-data (map-get? oracle-prices { asset: "STX" }))
    (synth-price u1000000)  ;; synth-usd pegged to $1.00 (6 decimals)
  )
    (match stx-price-data
      price-entry
        (if (is-eq synth-debt u0)
          u999999  ;; no debt = max ratio
          (/ (* (* collateral-stx (get price price-entry)) u10000)
             (* synth-debt synth-price)))
      u0  ;; no oracle data
    )
  )
)

;; Check if price data is fresh
(define-private (is-price-fresh (asset (string-ascii 12)))
  (match (map-get? oracle-prices { asset: asset })
    price-entry
      (< (- block-height (get last-updated-block price-entry)) PRICE-STALENESS-BLOCKS)
    false
  )
)

;; Update stability score for a user in a basin
(define-private (update-stability-score (basin-id uint) (user principal) (deposit uint))
  (let (
    (current-data (default-to
      { deposited-amount: u0, bTokens-held: u0, stability-score: u0, last-deposit-block: block-height }
      (map-get? basin-deposits { basin-id: basin-id, user: user })))
    (decayed-score (/ (* (get stability-score current-data) STABILITY-SCORE-DECAY) u10000))
    (contribution (if (> (get deposited-amount current-data) u0)
      (/ (* deposit u10000) (get deposited-amount current-data))
      u10000))
    (new-score (+ decayed-score contribution))
  )
    new-score
  )
)

;; Distribute fees: basin participants, stakers, treasury
;; Returns the fee amount distributed (plain uint, never fails)
(define-private (distribute-fees (fee-amount uint) (basin-id uint))
  (let (
    (basin-share (/ (* fee-amount BASIN-FEE-SHARE) u10000))
    (staker-share (/ (* fee-amount STAKER-FEE-SHARE) u10000))
    (treasury-share (/ (* fee-amount TREASURY-FEE-SHARE) u10000))
    (current-pool (default-to { unclaimed-fees: u0 } (map-get? basin-fee-pool { basin-id: basin-id })))
  )
    (map-set basin-fee-pool
      { basin-id: basin-id }
      { unclaimed-fees: (+ (get unclaimed-fees current-pool) basin-share) })
    (var-set total-protocol-fees (+ (var-get total-protocol-fees) fee-amount))
    fee-amount
  )
)

;; ============================================================
;; BASIN MANAGER
;; ============================================================

;; Initialize the protocol (one-time setup)
(define-public (initialize (treasury principal))
  (begin
    (asserts! (not (var-get initialized)) ERR-ALREADY-INITIALIZED)
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set treasury-address treasury)
    (var-set initialized true)
    (ok true)
  )
)

;; Create a new basin
(define-public (create-basin (name (string-ascii 32)))
  (let (
    (basin-id (+ (var-get total-basins) u1))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= basin-id MAX-BASINS) ERR-INVALID-AMOUNT)
    (map-set basins
      { basin-id: basin-id }
      {
        name: name,
        total-deposits: u0,
        total-bTokens: u0,
        accumulated-yield: u0,
        last-rebalance-block: block-height,
        active: true
      })
    (map-set basin-fee-pool { basin-id: basin-id } { unclaimed-fees: u0 })
    (var-set total-basins basin-id)
    (ok basin-id)
  )
)

;; Deposit STX into a basin, receive bTokens
(define-public (deposit-to-basin (basin-id uint) (amount uint))
  (let (
    (basin-data (unwrap! (map-get? basins { basin-id: basin-id }) ERR-BASIN-NOT-FOUND))
    (btokens-to-mint (calculate-btokens basin-id amount))
    (new-score (update-stability-score basin-id tx-sender amount))
    (current-deposit (default-to
      { deposited-amount: u0, bTokens-held: u0, stability-score: u0, last-deposit-block: block-height }
      (map-get? basin-deposits { basin-id: basin-id, user: tx-sender })))
  )
    (asserts! (get active basin-data) ERR-BASIN-NOT-FOUND)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Transfer STX from user to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Mint bTokens to user
    (try! (ft-mint? bToken btokens-to-mint tx-sender))
    ;; Update basin totals
    (map-set basins
      { basin-id: basin-id }
      (merge basin-data {
        total-deposits: (+ (get total-deposits basin-data) amount),
        total-bTokens: (+ (get total-bTokens basin-data) btokens-to-mint)
      }))
    ;; Update user deposit record
    (map-set basin-deposits
      { basin-id: basin-id, user: tx-sender }
      {
        deposited-amount: (+ (get deposited-amount current-deposit) amount),
        bTokens-held: (+ (get bTokens-held current-deposit) btokens-to-mint),
        stability-score: new-score,
        last-deposit-block: block-height
      })
    (ok btokens-to-mint)
  )
)

;; Withdraw from basin by burning bTokens
(define-public (withdraw-from-basin (basin-id uint) (btokens-amount uint))
  (let (
    (basin-data (unwrap! (map-get? basins { basin-id: basin-id }) ERR-BASIN-NOT-FOUND))
    (user-data (unwrap! (map-get? basin-deposits { basin-id: basin-id, user: tx-sender }) ERR-POSITION-NOT-FOUND))
    ;; Calculate proportional STX + yield to return
    (stx-to-return (/ (* btokens-amount (get total-deposits basin-data))
                      (get total-bTokens basin-data)))
    (yield-share (/ (* btokens-amount (get accumulated-yield basin-data))
                    (get total-bTokens basin-data)))
    (total-return (+ stx-to-return yield-share))
    (fee (/ (* total-return PROTOCOL-FEE-RATE) u10000))
    (net-return (- total-return fee))
  )
    (asserts! (>= (get bTokens-held user-data) btokens-amount) ERR-INVALID-AMOUNT)
    (asserts! (> btokens-amount u0) ERR-INVALID-AMOUNT)
    ;; Burn bTokens
    (try! (ft-burn? bToken btokens-amount tx-sender))
    ;; Send net STX back to user
    (try! (as-contract (stx-transfer? net-return tx-sender tx-sender)))
    ;; Update basin
    (map-set basins
      { basin-id: basin-id }
      (merge basin-data {
        total-deposits: (- (get total-deposits basin-data) stx-to-return),
        total-bTokens: (- (get total-bTokens basin-data) btokens-amount),
        accumulated-yield: (- (get accumulated-yield basin-data) yield-share)
      }))
    ;; Update user record
    (map-set basin-deposits
      { basin-id: basin-id, user: tx-sender }
      (merge user-data {
        deposited-amount: (- (get deposited-amount user-data) stx-to-return),
        bTokens-held: (- (get bTokens-held user-data) btokens-amount)
      }))
    ;; Distribute collected fee
    (distribute-fees fee basin-id)
    (ok net-return)
  )
)

;; Claim accumulated fees from a basin (pro-rata by bTokens held)
(define-public (claim-basin-fees (basin-id uint))
  (let (
    (basin-data (unwrap! (map-get? basins { basin-id: basin-id }) ERR-BASIN-NOT-FOUND))
    (user-data (unwrap! (map-get? basin-deposits { basin-id: basin-id, user: tx-sender }) ERR-POSITION-NOT-FOUND))
    (fee-pool (default-to { unclaimed-fees: u0 } (map-get? basin-fee-pool { basin-id: basin-id })))
    (user-share (if (> (get total-bTokens basin-data) u0)
      (/ (* (get bTokens-held user-data) (get unclaimed-fees fee-pool))
         (get total-bTokens basin-data))
      u0))
  )
    (asserts! (> user-share u0) ERR-INVALID-AMOUNT)
    (map-set basin-fee-pool
      { basin-id: basin-id }
      { unclaimed-fees: (- (get unclaimed-fees fee-pool) user-share) })
    (try! (as-contract (stx-transfer? user-share tx-sender tx-sender)))
    (ok user-share)
  )
)

;; ============================================================
;; DERIVATIVES ENGINE - CDP (Collateralized Debt Positions)
;; ============================================================

;; Open a CDP: lock STX, mint synth-usd
(define-public (open-cdp (collateral-stx uint) (synth-to-mint uint))
  (let (
    (position-id (var-get next-position-id))
    (ratio (get-collateral-ratio collateral-stx synth-to-mint))
    (user-count (default-to { count: u0 } (map-get? user-cdp-count { user: tx-sender })))
  )
    (asserts! (is-price-fresh "STX") ERR-ORACLE-STALE)
    (asserts! (> collateral-stx u0) ERR-INVALID-AMOUNT)
    (asserts! (> synth-to-mint u0) ERR-INVALID-AMOUNT)
    (asserts! (>= ratio MIN-COLLATERAL-RATIO) ERR-BELOW-MIN-COLLATERAL-RATIO)
    ;; Lock collateral in contract
    (try! (stx-transfer? collateral-stx tx-sender (as-contract tx-sender)))
    ;; Mint synthetic USD to user
    (try! (ft-mint? synth-usd synth-to-mint tx-sender))
    ;; Record position
    (map-set cdp-positions
      { position-id: position-id }
      {
        owner: tx-sender,
        collateral-amount: collateral-stx,
        synth-minted: synth-to-mint,
        collateral-ratio: ratio,
        opened-at-block: block-height,
        last-updated-block: block-height
      })
    (map-set user-cdp-count
      { user: tx-sender }
      { count: (+ (get count user-count) u1) })
    (var-set next-position-id (+ position-id u1))
    (ok position-id)
  )
)

;; Add collateral to an existing CDP
(define-public (add-collateral (position-id uint) (additional-stx uint))
  (let (
    (position (unwrap! (map-get? cdp-positions { position-id: position-id }) ERR-POSITION-NOT-FOUND))
    (new-collateral (+ (get collateral-amount position) additional-stx))
    (new-ratio (get-collateral-ratio new-collateral (get synth-minted position)))
  )
    (asserts! (is-eq (get owner position) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> additional-stx u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? additional-stx tx-sender (as-contract tx-sender)))
    (map-set cdp-positions
      { position-id: position-id }
      (merge position {
        collateral-amount: new-collateral,
        collateral-ratio: new-ratio,
        last-updated-block: block-height
      }))
    (ok new-ratio)
  )
)

;; Repay synth-usd debt and unlock collateral proportionally
(define-public (repay-cdp (position-id uint) (synth-to-repay uint))
  (let (
    (position (unwrap! (map-get? cdp-positions { position-id: position-id }) ERR-POSITION-NOT-FOUND))
    (repay-ratio (/ (* synth-to-repay u10000) (get synth-minted position)))
    (collateral-to-return (/ (* (get collateral-amount position) repay-ratio) u10000))
    (new-synth-debt (- (get synth-minted position) synth-to-repay))
    (new-collateral (- (get collateral-amount position) collateral-to-return))
    (new-ratio (get-collateral-ratio new-collateral new-synth-debt))
  )
    (asserts! (is-eq (get owner position) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (>= (get synth-minted position) synth-to-repay) ERR-INVALID-AMOUNT)
    ;; Burn synth-usd from user
    (try! (ft-burn? synth-usd synth-to-repay tx-sender))
    ;; Return proportional collateral
    (try! (as-contract (stx-transfer? collateral-to-return tx-sender tx-sender)))
    (map-set cdp-positions
      { position-id: position-id }
      (merge position {
        collateral-amount: new-collateral,
        synth-minted: new-synth-debt,
        collateral-ratio: new-ratio,
        last-updated-block: block-height
      }))
    (ok collateral-to-return)
  )
)

;; Liquidate an undercollateralized CDP
;; Liquidator repays debt, receives collateral + bonus
(define-public (liquidate-cdp (position-id uint))
  (let (
    (position (unwrap! (map-get? cdp-positions { position-id: position-id }) ERR-POSITION-NOT-FOUND))
    (current-ratio (get-collateral-ratio (get collateral-amount position) (get synth-minted position)))
    (debt (get synth-minted position))
    (collateral (get collateral-amount position))
    (penalty (/ (* collateral LIQUIDATION-PENALTY) u10000))
    (liquidator-receives (+ collateral penalty))
    ;; Cap at actual collateral available
    (actual-receives (if (> liquidator-receives collateral) collateral liquidator-receives))
  )
    (asserts! (is-price-fresh "STX") ERR-ORACLE-STALE)
    (asserts! (< current-ratio LIQUIDATION-THRESHOLD) ERR-UNDERCOLLATERALIZED)
    (asserts! (not (is-eq (get owner position) tx-sender)) ERR-NOT-AUTHORIZED)
    ;; Liquidator repays full debt
    (try! (ft-burn? synth-usd debt tx-sender))
    ;; Liquidator receives collateral (with bonus if available)
    (try! (as-contract (stx-transfer? actual-receives tx-sender tx-sender)))
    ;; Close the position
    (map-set cdp-positions
      { position-id: position-id }
      (merge position {
        collateral-amount: u0,
        synth-minted: u0,
        collateral-ratio: u0,
        last-updated-block: block-height
      }))
    (ok actual-receives)
  )
)

;; ============================================================
;; RISK ORACLE - Adaptive Risk Pricing
;; ============================================================

;; Report a new asset price (oracle authorized addresses)
;; In production this would use a multi-sig oracle committee
(define-public (report-price (asset (string-ascii 12)) (price uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> price u0) ERR-INVALID-AMOUNT)
    (map-set oracle-prices
      { asset: asset }
      {
        price: price,
        last-updated-block: block-height,
        reporter: tx-sender
      })
    (ok price)
  )
)

;; ============================================================
;; GOVERNANCE - BASIN Token Staking and Rewards
;; ============================================================

;; Stake BASIN governance tokens
(define-public (stake-gov-tokens (amount uint))
  (let (
    (current-stake (default-to
      { staked-amount: u0, stake-start-block: block-height, accumulated-rewards: u0 }
      (map-get? gov-stakes { staker: tx-sender })))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (ft-transfer? basin-gov amount tx-sender (as-contract tx-sender)))
    (map-set gov-stakes
      { staker: tx-sender }
      (merge current-stake {
        staked-amount: (+ (get staked-amount current-stake) amount),
        stake-start-block: block-height
      }))
    (ok amount)
  )
)

;; Unstake BASIN governance tokens
(define-public (unstake-gov-tokens (amount uint))
  (let (
    (current-stake (unwrap! (map-get? gov-stakes { staker: tx-sender }) ERR-POSITION-NOT-FOUND))
  )
    (asserts! (>= (get staked-amount current-stake) amount) ERR-INVALID-AMOUNT)
    (try! (as-contract (ft-transfer? basin-gov amount tx-sender tx-sender)))
    (map-set gov-stakes
      { staker: tx-sender }
      (merge current-stake {
        staked-amount: (- (get staked-amount current-stake) amount)
      }))
    (ok amount)
  )
)

;; Mint initial governance tokens (admin only)
(define-public (mint-gov-tokens (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ft-mint? basin-gov amount recipient)
  )
)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

;; Get basin info
(define-read-only (get-basin (basin-id uint))
  (map-get? basins { basin-id: basin-id })
)

;; Get user deposit info in a basin
(define-read-only (get-user-basin-info (basin-id uint) (user principal))
  (map-get? basin-deposits { basin-id: basin-id, user: user })
)

;; Get CDP position
(define-read-only (get-cdp (position-id uint))
  (map-get? cdp-positions { position-id: position-id })
)

;; Get current collateral ratio for a CDP
(define-read-only (get-cdp-ratio (position-id uint))
  (match (map-get? cdp-positions { position-id: position-id })
    position (some (get-collateral-ratio (get collateral-amount position) (get synth-minted position)))
    none
  )
)

;; Get oracle price for an asset
(define-read-only (get-price (asset (string-ascii 12)))
  (map-get? oracle-prices { asset: asset })
)

;; Get governance stake info
(define-read-only (get-stake-info (staker principal))
  (map-get? gov-stakes { staker: staker })
)

;; Get basin fee pool
(define-read-only (get-basin-fees (basin-id uint))
  (map-get? basin-fee-pool { basin-id: basin-id })
)

;; Get total basins
(define-read-only (get-total-basins)
  (var-get total-basins)
)

;; Get total protocol fees collected
(define-read-only (get-total-fees)
  (var-get total-protocol-fees)
)

;; Check if a CDP is liquidatable
(define-read-only (is-liquidatable (position-id uint))
  (match (map-get? cdp-positions { position-id: position-id })
    position
      (< (get-collateral-ratio (get collateral-amount position) (get synth-minted position))
         LIQUIDATION-THRESHOLD)
    false
  )
)

;; Get bToken balance
(define-read-only (get-btoken-balance (user principal))
  (ft-get-balance bToken user)
)

;; Get synth-usd balance
(define-read-only (get-synth-balance (user principal))
  (ft-get-balance synth-usd user)
)

;; Get BASIN governance token balance
(define-read-only (get-gov-balance (user principal))
  (ft-get-balance basin-gov user)
)

;; Get next position ID
(define-read-only (get-next-position-id)
  (var-get next-position-id)
)
