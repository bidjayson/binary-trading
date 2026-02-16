;; CarbonChain - Decentralized Carbon Credit Platform
;; A smart contract for community-verified carbon credits with prediction markets

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-project-not-active (err u106))
(define-constant err-prediction-closed (err u107))
(define-constant err-already-verified (err u108))

;; Data Variables
(define-data-var credit-token-supply uint u0)
(define-data-var next-project-id uint u1)
(define-data-var next-prediction-id uint u1)

;; Data Maps
(define-map projects
    { project-id: uint }
    {
        creator: principal,
        name: (string-ascii 100),
        location: (string-ascii 100),
        project-type: (string-ascii 50),
        target-credits: uint,
        minted-credits: uint,
        trust-score: uint,
        is-active: bool,
        created-at: uint
    }
)

(define-map carbon-credits
    { holder: principal }
    { balance: uint }
)

(define-map project-guardians
    { project-id: uint, guardian: principal }
    { 
        stake-amount: uint,
        verification-count: uint,
        reputation-score: uint
    }
)

(define-map verification-reports
    { project-id: uint, report-id: uint }
    {
        guardian: principal,
        iot-data-hash: (string-ascii 64),
        satellite-data-hash: (string-ascii 64),
        community-verified: bool,
        timestamp: uint,
        credits-proposed: uint
    }
)

(define-map prediction-markets
    { prediction-id: uint }
    {
        project-id: uint,
        outcome-description: (string-ascii 200),
        target-value: uint,
        deadline: uint,
        total-yes-stake: uint,
        total-no-stake: uint,
        is-settled: bool,
        actual-outcome: bool
    }
)

(define-map user-predictions
    { prediction-id: uint, user: principal }
    {
        stake-amount: uint,
        predicted-yes: bool,
        claimed: bool
    }
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
    (map-get? projects { project-id: project-id })
)

(define-read-only (get-carbon-balance (holder principal))
    (default-to { balance: u0 }
        (map-get? carbon-credits { holder: holder })
    )
)

(define-read-only (get-guardian-info (project-id uint) (guardian principal))
    (map-get? project-guardians { project-id: project-id, guardian: guardian })
)

(define-read-only (get-prediction-market (prediction-id uint))
    (map-get? prediction-markets { prediction-id: prediction-id })
)

(define-read-only (get-user-prediction (prediction-id uint) (user principal))
    (map-get? user-predictions { prediction-id: prediction-id, user: user })
)

(define-read-only (get-total-supply)
    (var-get credit-token-supply)
)

;; Public functions

;; Register a new environmental project
(define-public (register-project 
    (name (string-ascii 100))
    (location (string-ascii 100))
    (project-type (string-ascii 50))
    (target-credits uint))
    (let
        (
            (project-id (var-get next-project-id))
        )
        (map-set projects
            { project-id: project-id }
            {
                creator: tx-sender,
                name: name,
                location: location,
                project-type: project-type,
                target-credits: target-credits,
                minted-credits: u0,
                trust-score: u50,
                is-active: true,
                created-at: block-height
            }
        )
        (var-set next-project-id (+ project-id u1))
        (ok project-id)
    )
)

;; Register as a project guardian with stake
(define-public (register-guardian (project-id uint) (stake-amount uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
        )
        (asserts! (get is-active project) err-project-not-active)
        (asserts! (> stake-amount u0) err-invalid-amount)
        
        (map-set project-guardians
            { project-id: project-id, guardian: tx-sender }
            {
                stake-amount: stake-amount,
                verification-count: u0,
                reputation-score: u100
            }
        )
        (ok true)
    )
)

;; Submit verification report
(define-public (submit-verification 
    (project-id uint)
    (report-id uint)
    (iot-data-hash (string-ascii 64))
    (satellite-data-hash (string-ascii 64))
    (credits-proposed uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (guardian-info (unwrap! (map-get? project-guardians 
                { project-id: project-id, guardian: tx-sender }) err-unauthorized))
        )
        (asserts! (get is-active project) err-project-not-active)
        
        (map-set verification-reports
            { project-id: project-id, report-id: report-id }
            {
                guardian: tx-sender,
                iot-data-hash: iot-data-hash,
                satellite-data-hash: satellite-data-hash,
                community-verified: false,
                timestamp: block-height,
                credits-proposed: credits-proposed
            }
        )
        
        ;; Update guardian verification count
        (map-set project-guardians
            { project-id: project-id, guardian: tx-sender }
            (merge guardian-info { verification-count: (+ (get verification-count guardian-info) u1) })
        )
        
        (ok true)
    )
)

;; Mint carbon credits after verification consensus
(define-public (mint-carbon-credits (project-id uint) (amount uint) (recipient principal))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (current-balance (get balance (get-carbon-balance recipient)))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (get is-active project) err-project-not-active)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Update project minted credits
        (map-set projects
            { project-id: project-id }
            (merge project { minted-credits: (+ (get minted-credits project) amount) })
        )
        
        ;; Mint credits to recipient
        (map-set carbon-credits
            { holder: recipient }
            { balance: (+ current-balance amount) }
        )
        
        ;; Update total supply
        (var-set credit-token-supply (+ (var-get credit-token-supply) amount))
        
        (ok amount)
    )
)

;; Transfer carbon credits
(define-public (transfer-credits (amount uint) (recipient principal))
    (let
        (
            (sender-balance (get balance (get-carbon-balance tx-sender)))
            (recipient-balance (get balance (get-carbon-balance recipient)))
        )
        (asserts! (>= sender-balance amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        
        (map-set carbon-credits
            { holder: tx-sender }
            { balance: (- sender-balance amount) }
        )
        
        (map-set carbon-credits
            { holder: recipient }
            { balance: (+ recipient-balance amount) }
        )
        
        (ok true)
    )
)

;; Create prediction market for project outcome
(define-public (create-prediction-market
    (project-id uint)
    (outcome-description (string-ascii 200))
    (target-value uint)
    (deadline uint))
    (let
        (
            (prediction-id (var-get next-prediction-id))
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
        )
        (asserts! (get is-active project) err-project-not-active)
        
        (map-set prediction-markets
            { prediction-id: prediction-id }
            {
                project-id: project-id,
                outcome-description: outcome-description,
                target-value: target-value,
                deadline: deadline,
                total-yes-stake: u0,
                total-no-stake: u0,
                is-settled: false,
                actual-outcome: false
            }
        )
        
        (var-set next-prediction-id (+ prediction-id u1))
        (ok prediction-id)
    )
)

;; Stake on prediction market outcome
(define-public (stake-prediction (prediction-id uint) (amount uint) (predict-yes bool))
    (let
        (
            (market (unwrap! (map-get? prediction-markets { prediction-id: prediction-id }) err-not-found))
            (sender-balance (get balance (get-carbon-balance tx-sender)))
        )
        (asserts! (>= sender-balance amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (not (get is-settled market)) err-prediction-closed)
        (asserts! (<= block-height (get deadline market)) err-prediction-closed)
        
        ;; Deduct stake from user's carbon credits
        (map-set carbon-credits
            { holder: tx-sender }
            { balance: (- sender-balance amount) }
        )
        
        ;; Record user prediction
        (map-set user-predictions
            { prediction-id: prediction-id, user: tx-sender }
            {
                stake-amount: amount,
                predicted-yes: predict-yes,
                claimed: false
            }
        )
        
        ;; Update market totals
        (map-set prediction-markets
            { prediction-id: prediction-id }
            (merge market {
                total-yes-stake: (if predict-yes 
                    (+ (get total-yes-stake market) amount)
                    (get total-yes-stake market)),
                total-no-stake: (if predict-yes
                    (get total-no-stake market)
                    (+ (get total-no-stake market) amount))
            })
        )
        
        (ok true)
    )
)

;; Settle prediction market (owner only)
(define-public (settle-prediction (prediction-id uint) (actual-outcome bool))
    (let
        (
            (market (unwrap! (map-get? prediction-markets { prediction-id: prediction-id }) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (not (get is-settled market)) err-already-verified)
        
        (map-set prediction-markets
            { prediction-id: prediction-id }
            (merge market {
                is-settled: true,
                actual-outcome: actual-outcome
            })
        )
        
        (ok true)
    )
)

;; Claim prediction market winnings
(define-public (claim-prediction-winnings (prediction-id uint))
    (let
        (
            (market (unwrap! (map-get? prediction-markets { prediction-id: prediction-id }) err-not-found))
            (user-pred (unwrap! (map-get? user-predictions 
                { prediction-id: prediction-id, user: tx-sender }) err-not-found))
            (current-balance (get balance (get-carbon-balance tx-sender)))
        )
        (asserts! (get is-settled market) err-prediction-closed)
        (asserts! (not (get claimed user-pred)) err-already-verified)
        (asserts! (is-eq (get predicted-yes user-pred) (get actual-outcome market)) err-unauthorized)
        
        (let
            (
                (total-winning-stake (if (get actual-outcome market)
                    (get total-yes-stake market)
                    (get total-no-stake market)))
                (total-losing-stake (if (get actual-outcome market)
                    (get total-no-stake market)
                    (get total-yes-stake market)))
                (user-stake (get stake-amount user-pred))
                (proportional-winnings (/ (* user-stake total-losing-stake) total-winning-stake))
                (total-payout (+ user-stake proportional-winnings))
            )
            
            ;; Mark as claimed
            (map-set user-predictions
                { prediction-id: prediction-id, user: tx-sender }
                (merge user-pred { claimed: true })
            )
            
            ;; Transfer winnings
            (map-set carbon-credits
                { holder: tx-sender }
                { balance: (+ current-balance total-payout) }
            )
            
            (ok total-payout)
        )
    )
)

;; Update project trust score (owner only)
(define-public (update-trust-score (project-id uint) (new-score uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set projects
            { project-id: project-id }
            (merge project { trust-score: new-score })
        )
        
        (ok true)
    )
)