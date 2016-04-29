;------------------------------------------------------------------------------
;                  ***** MINIMAX.LSP *****
;Generalized recursive minimax routine with alpha-beta pruning.
;
;Author: Dr. John M. Weiss
;Modified by: Stephanie Athow, Dan Nix
;
;Class:  SDSM&T CSC447/547 Artificial Intelligence
;Date:   26 April 2016
;
;Usage:  (minimax position ply curr_depth player mm_player alpha beta)
;        position: board state to be evaluated
;		ply: depth of game tree to search
;		curr_depth: depth of current node
;		player: color of person making move
;		mm_player: MAX or MIN player for minimax
;		alpha: value of best MAX player position
;		beta: value of best MIN player position
;
;Returns:  (value path)
;          where value is the backed-up value from evaluation of leaf nodes,
;          and path is the path to the desired leaf node.
;
;Functions called:
;       (deep-enough ply curr_depth) -
;          predicate that returns T if the current position has reached
;          the desired search depth, NIL otherwise.
;
;       (move-generator position player) -
;          generates successors to the position.
;
;       (static position player) -
;          applies the static evaluation function to the position.
;------------------------------------------------------------------------------
(defun minimax (position ply curr_depth player mm_player alpha beta)

    (format t "Made it to minimax~%")
    (format t "Pos: ~S~%" position)
    (format t "Player ~s~%" player)
    (format t "Move gen: ~s~%" (move-generator position player))
    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    (if (or (deep-enough ply curr_depth) (null (move-generator position player)))
        (list (static position player) nil)

        ; otherwise, generate successors and run minimax recursively
        (let
            (
                ; generate list of sucessor positions
                (successors (move-generator position player))

                ; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                best-score

                ; other local variables
                succ-value
                succ-score
            )

            (if (equal mm_player 'MAX) (setf best-score -1000000) (setf best-score 1000000) )

            (format t "Position: ~s~%" position)
            (format t "Player: ~s~%" player)
            (format t "SUCCESSORS: ~s~%" successors)
            ; explore possible moves by looping through successor positions
            (dolist (successor successors)
                (format "SUCC")

                ; Decide if we need to make a recursive call, or employ alpha beta pruning
                (cond 
                    ((not (prune alpha beta mm_player))
                        ; perform recursive DFS exploration of game tree
                        (setq succ-value (minimax successor ply (1+ curr_depth) player (swap-player mm_player) alpha beta))

                        (setq succ-score (car succ-value))

                        (cond 
                            ( (equal mm_player 'MAX)
                                ; update best value and path if a better move is found
                                ; (note that path is being stored in reverse order)
                                (when (> succ-score best-score)
                                      (setq best-score succ-score)
                                      (setq best-path (cons successor (cdr succ-value)))
                                      (setf alpha succ-score)
                                )
                            )
                            ( (equal mm_player 'MIN)
                                ; update best value and path if a better move is found
                                ; (note that path is being stored in reverse order)
                                (when (< succ-score best-score)
                                      (setq best-score succ-score)
                                      (setq best-path (cons successor (cdr succ-value)))
                                      (setf beta succ-score)
                                )
                            )
                        ); end cond MAX or MIN
                    ) ; end cond not block 
                ) ;end cond recursion call
            ) ;end do list

            (format t "Path: ~s~%" best-path)
            ; return (value path) list when done
            (list best-score best-path)
        )
    )
)

;------------------------------------------------------------------------------
;Function: Deep-Enough
;
;Author: Stephanie Athow
;
;Description:
;	predicate that returns T if the current position has reached the desired
;	search depth, NIL otherwise.

;Date: 26 April 2016
;
;Usage: (deep-enough ply depth)
;	ply:	maximum depth to search for minimax
;	depth:	current node depth
;
;Returns:
;	t - current node is at ply depth
;	nil - node is not at ply depth
;------------------------------------------------------------------------------
(defun deep-enough (ply depth)
  (if (= ply depth) T NIL)
)


;------------------------------------------------------------------------------
;Function: Prune
;
;Author: Stephanie Athow, Dan Nix
;
;Description: swaps the indicator of MAX/MIN player
;
;Date: 26 April 2016
;
;Usage: (swap-player mm_player)
;	mm_player - indicates MIN/MAX player
;
;Returns:
;	mm_player - swapped indicator of MAX/MIN player
;------------------------------------------------------------------------------
(defun swap-player (mm_player)
    (if (equal mm_player 'MAX) (setf mm_player 'MIN) (setf mm_player 'MAX))
)


;------------------------------------------------------------------------------
;Function: Prune
;
;Author: Dan Nix
;
;Description:
;	Applies check for alpha-beta pruning

;Date: 26 April 2016
;
;Usage: (prune alpha beta mm_player)
;	alpha - value of best MAX node
;	beta - value of best MIN node
;	mm_player - indicates MIN/MAX player
;
;Returns:
;	t - alpha-beta prune
;	nil - continue search
;------------------------------------------------------------------------------
(defun prune (alpha beta mm_player)

    (cond 
        ; If we're in a MAX node
        ((equal mm_player 'MAX)
            ; Prune if alpha >= beta
            (>= alpha beta)
        )
        ; If we're in a MIN node
        (t
            ; Prune if beta <= alpha
            (<= beta alpha)
        )
    )
)
