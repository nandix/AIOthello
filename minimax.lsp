#|
                  ***** MINIMAX.LSP *****

Generalized recursive minimax routine.

Author: Dr. John M. Weiss
Class:	SDSM&T CSC447/547 Artificial Intelligence
Date: 	Spring 2016

Usage:    (minimax position depth)
          where position is the position to be evaluated,
          and depth is the search depth (number of plys).

Returns:  (value path)
          where value is the backed-up value from evaluation of leaf nodes,
          and path is the path to the desired leaf node.

Functions called:

          (deepenough depth) -
              predicate that returns T if the current position has reached
              the desired search depth, NIL otherwise.

          (move-generator position player) -
              generates successors to the position.

          (static position player) -
              applies the static evaluation function to the position.

          Note: these functions may need additional arguments.
|#

(defun minimax (position ply curr_depth player mm_player )

    ; if we have searched deep enough, or there are no successors,
    ; return position evaluation and nil for the path
    (if (or (deep-enough ply curr_depth) (null (move-generator position player)))
        (list (static position player) nil)

        ; otherwise, generate successors and run minimax recursively
        (let
            (
                ; generate list of sucessor positions
                (successors (move-generator position max_player))

                ; initialize current best path to nil
                (best-path nil)

                ; initialize current best score to negative infinity
                (best-score -1000000)

                ; other local variables
                succ-value
                succ-score
            )

            ; explore possible moves by looping through successor positions
            (dolist (successor successors)

                ; perform recursive DFS exploration of game tree
                (setq succ-value (minimax successor ply (1- depth) player))

                ; change sign every ply to reflect alternating selection
                ; of MAX/MIN player (maximum/minimum value)
                (setq succ-score (- (car succ-value)))

                ; update best value and path if a better move is found
                ; (note that path is being stored in reverse order)
				; alpha-beta pruning:
				;	alpha (max) cutoff: stop search below MIN node when
				;		beta <= alpha of ancestor
				;	beta (min) cutoff: stop search below MAX node when
				;		alpha >= beta of ancestor

				(cond
	                (when (> succ-score 0)
						(when (> succ-score best-score)
		                     (setq best-score succ-score)
		                     (setq best-path (cons successor (cdr succ-value)))
	                	)
					)
					(when ( <= succ-score 0)
						(when (< succ-score best-score)
							(setq best-score succ-score)
							(setq best-path (cons successor (cdr succ-value)))
						)
					)
				)
           ) 
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
