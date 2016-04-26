;------------------------------------------------------------------------------
; Function:     make-move
;
; Author:       Daniel Nix
;
; Description:  Prompts a user for a row/col for the next move. Loops until
;				a valid move has been made
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun make-move (boardState player ply)

	(let (movedBoard) 
		(setf movedBoard 
			(car (nth 1 (minimax *gameBoard* ply '0 player 'MAX -1000000 1000000) ) ) )

		(newboard-to-move *gameBoard* movedBoard)
	)
)

;------------------------------------------------------------------------------
; Function:     newboard-to-move
;
; Author:       Daniel Nix
;
; Description:  Prompts a user for a row/col for the next move. Loops until
;				a valid move has been made
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun newboard-to-move (origBoard newBoard)

	(let (
			c1 
			c2 
			row
			col
			(movedIndex 0)
		)
		; For each element in the puzzles
		(dotimes (i 64)

			; Get the elements of the boards to compare
			(setf c1 (nth i origBoard))
			(setf c2 (nth i newBoard))

			; If the piece changed from - to 'b' or 'w', mark this
			;	as the moved index
			(if (moved-here? c1 c2)
				(setf movedIndex i)
			)

		)

		; Convert the index to column and row
		(setf col  (mod movedIndex 8))
		
		(setf row
			(1+ (/ (- movedIndex col) 8))
		)

		(incf col)

		(list row col)
	)
)
;------------------------------------------------------------------------------
; Function:     moved-here?
;
; Author:       Daniel Nix
;
; Description:  Prompts a user for a row/col for the next move. Loops until
;				a valid move has been made
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun moved-here? (c1 c2)

	(cond
		(
			(and 
				(equal c1 '-)
				(or 
					(equal c2 'b)
					(equal c2 'w)
				)
			)
			't
		)
		(t 'NIL)

	)

)