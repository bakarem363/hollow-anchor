;; HollowAnchor - Temporal Stake Anchoring DAO Governance Platform
;; Clarity Version 2, Epoch 2.1
;; A next-generation DAO governance platform with temporal stake anchoring,
;; hollow voting mechanisms, and multi-dimensional reputation scoring.

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-voting-closed (err u104))
(define-constant err-voting-active (err u105))
(define-constant err-insufficient-stake (err u106))
(define-constant err-invalid-duration (err u107))
(define-constant err-proposal-executed (err u108))

;; Minimum stake required to create proposals (in microSTX)
(define-constant min-stake-to-propose u1000000)

;; Voting period in blocks (approximately 1 week)
(define-constant default-voting-period u1008)

;; Data Variables
(define-data-var proposal-count uint u0)
(define-data-var total-staked uint u0)

;; Data Maps

;; Temporal Stake Anchoring - tracks user stakes with time locks
(define-map user-stakes
    principal
    {
        amount: uint,
        lock-duration: uint,
        locked-at: uint,
        stake-multiplier: uint
    }
)

;; Reputation System - multi-dimensional scoring
(define-map user-reputation
    principal
    {
        voting-accuracy: uint,
        participation-score: uint,
        contribution-points: uint,
        total-reputation: uint
    }
)

;; Proposals
(define-map proposals
    uint
    {
        proposer: principal,
        title: (string-ascii 256),
        description: (string-ascii 1024),
        start-block: uint,
        end-block: uint,
        yes-votes: uint,
        no-votes: uint,
        total-voting-power: uint,
        executed: bool,
        passed: bool
    }
)

;; Vote commitments (hollow voting - cryptographic commitment)
(define-map vote-commitments
    { proposal-id: uint, voter: principal }
    {
        commitment-hash: (buff 32),
        revealed: bool,
        vote-value: bool,
        voting-power: uint
    }
)

;; User votes tracking
(define-map user-votes
    { proposal-id: uint, voter: principal }
    bool
)

;; Contribution Graph - tracks diverse community participation
(define-map contribution-metrics
    principal
    {
        proposals-created: uint,
        votes-cast: uint,
        discussions-participated: uint,
        peer-reviews-completed: uint
    }
)

;; Read-only functions

;; Get user stake information
(define-read-only (get-user-stake (user principal))
    (default-to
        { amount: u0, lock-duration: u0, locked-at: u0, stake-multiplier: u1 }
        (map-get? user-stakes user)
    )
)

;; Get user reputation
(define-read-only (get-user-reputation (user principal))
    (default-to
        { voting-accuracy: u0, participation-score: u0, contribution-points: u0, total-reputation: u0 }
        (map-get? user-reputation user)
    )
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

;; Get current proposal count
(define-read-only (get-proposal-count)
    (ok (var-get proposal-count))
)

;; Get total staked amount
(define-read-only (get-total-staked)
    (ok (var-get total-staked))
)

;; Calculate voting power based on stake and duration
(define-read-only (calculate-voting-power (user principal))
    (let
        (
            (stake (get-user-stake user))
            (amount (get amount stake))
            (multiplier (get stake-multiplier stake))
        )
        (ok (* amount multiplier))
    )
)

;; Check if user has voted on proposal
(define-read-only (has-voted (proposal-id uint) (user principal))
    (is-some (map-get? user-votes { proposal-id: proposal-id, voter: user }))
)

;; Get user contribution metrics
(define-read-only (get-contribution-metrics (user principal))
    (default-to
        { proposals-created: u0, votes-cast: u0, discussions-participated: u0, peer-reviews-completed: u0 }
        (map-get? contribution-metrics user)
    )
)

;; Public functions

;; Temporal Stake Anchoring - lock tokens with time-based multiplier
(define-public (stake-tokens (amount uint) (lock-duration uint))
    (let
        (
            (current-stake (get-user-stake tx-sender))
            (current-amount (get amount current-stake))
            (new-amount (+ current-amount amount))
            ;; Calculate stake multiplier based on lock duration
            ;; 1x for < 90 days, 2x for 90-180 days, 3x for 180-365 days, 4x for > 365 days
            (multiplier (if (>= lock-duration u52560)
                            u4
                            (if (>= lock-duration u26280)
                                u3
                                (if (>= lock-duration u13140)
                                    u2
                                    u1))))
        )
        (asserts! (> amount u0) err-insufficient-stake)
        (asserts! (> lock-duration u0) err-invalid-duration)
        
        ;; Update user stake
        (map-set user-stakes
            tx-sender
            {
                amount: new-amount,
                lock-duration: lock-duration,
                locked-at: block-height,
                stake-multiplier: multiplier
            }
        )
        
        ;; Update total staked
        (var-set total-staked (+ (var-get total-staked) amount))
        
        (ok true)
    )
)

;; Create a new proposal
(define-public (create-proposal (title (string-ascii 256)) (description (string-ascii 1024)))
    (let
        (
            (stake (get-user-stake tx-sender))
            (stake-amount (get amount stake))
            (new-proposal-id (+ (var-get proposal-count) u1))
        )
        (asserts! (>= stake-amount min-stake-to-propose) err-insufficient-stake)
        
        ;; Create proposal
        (map-set proposals
            new-proposal-id
            {
                proposer: tx-sender,
                title: title,
                description: description,
                start-block: block-height,
                end-block: (+ block-height default-voting-period),
                yes-votes: u0,
                no-votes: u0,
                total-voting-power: u0,
                executed: false,
                passed: false
            }
        )
        
        ;; Update proposal count
        (var-set proposal-count new-proposal-id)
        
        ;; Update contribution metrics
        (let
            (
                (metrics (get-contribution-metrics tx-sender))
            )
            (map-set contribution-metrics
                tx-sender
                (merge metrics { proposals-created: (+ (get proposals-created metrics) u1) })
            )
        )
        
        (ok new-proposal-id)
    )
)

;; Hollow Voting - Commit vote with cryptographic hash
(define-public (commit-vote (proposal-id uint) (commitment-hash (buff 32)))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) err-not-found))
            (voting-power (unwrap! (calculate-voting-power tx-sender) err-unauthorized))
        )
        (asserts! (< block-height (get end-block proposal)) err-voting-closed)
        (asserts! (not (has-voted proposal-id tx-sender)) err-already-voted)
        (asserts! (> voting-power u0) err-insufficient-stake)
        
        ;; Store vote commitment
        (map-set vote-commitments
            { proposal-id: proposal-id, voter: tx-sender }
            {
                commitment-hash: commitment-hash,
                revealed: false,
                vote-value: false,
                voting-power: voting-power
            }
        )
        
        ;; Mark as voted
        (map-set user-votes
            { proposal-id: proposal-id, voter: tx-sender }
            true
        )
        
        (ok true)
    )
)

;; Reveal vote after voting period (simplified version)
(define-public (reveal-vote (proposal-id uint) (vote bool) (nonce (buff 32)))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) err-not-found))
            (commitment (unwrap! (map-get? vote-commitments { proposal-id: proposal-id, voter: tx-sender }) err-not-found))
            (voting-power (get voting-power commitment))
        )
        (asserts! (>= block-height (get end-block proposal)) err-voting-active)
        (asserts! (not (get revealed commitment)) err-already-voted)
        
        ;; Update vote commitment with revealed value
        (map-set vote-commitments
            { proposal-id: proposal-id, voter: tx-sender }
            (merge commitment { revealed: true, vote-value: vote })
        )
        
        ;; Update proposal vote counts
        (map-set proposals
            proposal-id
            (merge proposal
                {
                    yes-votes: (if vote (+ (get yes-votes proposal) voting-power) (get yes-votes proposal)),
                    no-votes: (if vote (get no-votes proposal) (+ (get no-votes proposal) voting-power)),
                    total-voting-power: (+ (get total-voting-power proposal) voting-power)
                }
            )
        )
        
        ;; Update contribution metrics
        (let
            (
                (metrics (get-contribution-metrics tx-sender))
            )
            (map-set contribution-metrics
                tx-sender
                (merge metrics { votes-cast: (+ (get votes-cast metrics) u1) })
            )
        )
        
        (ok true)
    )
)

;; Execute proposal after voting ends
(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) err-not-found))
        )
        (asserts! (>= block-height (get end-block proposal)) err-voting-active)
        (asserts! (not (get executed proposal)) err-proposal-executed)
        
        ;; Determine if proposal passed (simple majority)
        (let
            (
                (passed (> (get yes-votes proposal) (get no-votes proposal)))
            )
            (map-set proposals
                proposal-id
                (merge proposal { executed: true, passed: passed })
            )
            
            (ok passed)
        )
    )
)

;; Governance Mining - reward participation
(define-public (claim-governance-rewards)
    (let
        (
            (metrics (get-contribution-metrics tx-sender))
            (reputation (get-user-reputation tx-sender))
            ;; Calculate reward based on contribution
            (reward-points (+ 
                (* (get proposals-created metrics) u100)
                (* (get votes-cast metrics) u50)
                (* (get discussions-participated metrics) u25)
                (* (get peer-reviews-completed metrics) u75)
            ))
        )
        ;; Update reputation with new contribution points
        (map-set user-reputation
            tx-sender
            (merge reputation 
                { 
                    contribution-points: (+ (get contribution-points reputation) reward-points),
                    total-reputation: (+ (get total-reputation reputation) reward-points)
                }
            )
        )
        
        (ok reward-points)
    )
)

;; Update participation metrics (called by discussion/review systems)
(define-public (record-discussion-participation)
    (let
        (
            (metrics (get-contribution-metrics tx-sender))
        )
        (map-set contribution-metrics
            tx-sender
            (merge metrics { discussions-participated: (+ (get discussions-participated metrics) u1) })
        )
        (ok true)
    )
)

;; Record peer review completion
(define-public (record-peer-review)
    (let
        (
            (metrics (get-contribution-metrics tx-sender))
        )
        (map-set contribution-metrics
            tx-sender
            (merge metrics { peer-reviews-completed: (+ (get peer-reviews-completed metrics) u1) })
        )
        (ok true)
    )
)

;; Initialize contract
(begin
    (var-set proposal-count u0)
    (var-set total-staked u0)
)
