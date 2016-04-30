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
		; Set the board to the next step indicated by minimax
		(setf movedBoard 
			(car (nth 1 (minimax boardState ply '0 player 'MAX -1000000 1000000) ) ) )

		; Convert the new game board to a row/col position to move
		(newboard-to-move boardState movedBoard) 
	)
)

;------------------------------------------------------------------------------
; Function:     newboard-to-move
;
; Author:       Daniel Nix
;
; Description:  Takes a previous and new board configuration and determines
;					where the most recent move was placed
;
; Parameters:   origBoard: original board state
;				newBoard: new board state
;
; Return:       move: 1 indexed (row col) list indicating where the move was 
;						taken
;------------------------------------------------------------------------------
(defun newboard-to-move (origBoard newBoard)

	(let (
			c1 
			c2 
			(row 'NIL)
			(col 'NIL)
			(movedIndex -1)
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

		(cond 
			((= movedIndex -1)
				(return-from newboard-to-move 'NIL)
			)

			(t
				; Convert the index to column and row
				(setf col  (mod movedIndex 8))
				
				; Compute 1 based row
				(setf row
					(1+ (/ (- movedIndex col) 8))
				)

				; Convert the column to 1 based index
				(incf col)

				(list row col)
			)
		
		)
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
			; If the previous board was blank and the 
			;	new board holds a piece, return true. Otherwise return false
			(and 
				(equal c1 '-)
				(not (equal c2 '-))
			)
			't
		)
		(t 'NIL)

	)

)
