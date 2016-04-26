#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:  SDSM&T CSC447/547 Artificial Intelligence
Date:   Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deep-enough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position) -
              generates successors to the position.

          (static position) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#

(defun minimax (position ply curr_depth player mm_player alpha beta)

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

            ; explore possible moves by looping through successor positions
            (dolist (successor successors)

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

            ; return (value path) list when done
            (list best-score best-path)
        )
    )
)

;(deep-enough depth) -
; predicate that returns T if the current position has reached
; the desired search depth, NIL otherwise.
(defun deep-enough (ply depth)
  (if (= ply depth) T NIL)
)

; swaps min and max player label
(defun swap-player (mm_player)
    (if (equal mm_player 'MAX) (setf mm_player 'MIN) (setf mm_player 'MAX))
)

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
