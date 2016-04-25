
; Initialize globals
(defvar *gameBoard*)
(defvar *firstMove* 'NIL)
(defvar *color* 'black)

(load "printBoard.lsp")
(load "move_generator.lsp")
(load "static_eval.lsp")
(load "minimax.lsp")

;------------------------------------------------------------------------------
; Function:     othello
;
; Author:       Daniel Nix
;
; Description:  Othello is the starting function for the othello program.
;				It initializes the board and handles the initialization
;				sequence, asking the user what color they want to play
;				as and whether they want to play first.
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun othello ()

	; Initialize the game board
	(othello-init)

	(printBoard)

	(loop while (not (game-is-done)) do

		(format t "Player ~s, your turn!~%" *color*)
		(make-user-move)
		(printBoard)
		(trade-turns)
	)

	(print-game-stats)


)

;------------------------------------------------------------------------------
; Function:     othello-init
;
; Author:       Daniel Nix
;
; Description:  othello-init is responsible for setting up a new game.
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun othello-init ()

	(setf *gameBoard* 
		'( 	- - - - - - - -
			- - - - - - - -
			- - - - - - - -
			- - - W B - - -
			- - - B W - - -
			- - - - - - - -
			- - - - - - - -
			- - - - - - - - ))

	; Ask the user if they would like to move first
	(format t "Would you like to move first [y/n]? ")
	(setf *firstMove* (read))
	(if (equal *firstMove* 'n)
		(setf *firstMove* 'NIL)
		(setf *firstMove* 't)
	)
	(format t "What color would you like to be [black/white]? ")
	(setf *color* (read))

	;If the color is black, set it to b. Otherwise, set to w.
	(if (equal *color* 'black)
		(setf *color* 'b)
		(setf *color* 'w)
	)


	(format t "OK! You will be playing ~s. When asked for your move, please enter the row~%
	and column in which you would like to place a ~s stone. Remember, you must~%
	outflank at least one White stone, or forfeit your move.~%~%" *color* *color*)
)

;------------------------------------------------------------------------------
; Function:     make-user-move
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
(defun make-user-move ()

	(let 
		(
		(row 'NIL) 
		(col 'NIL) 
		(valid 'NIL)
		)

		(loop while (not valid) do

			(format t "What is your move [row col]? ")
			(setf row (read))
			(setf col (read))
			(setf valid (validate-move *gameBoard* (list row col) *color*) )
			(if (not valid)
				(format t "Invalid move, please try again~%")
			)
		)
		(format t "Row is: ~d, col: ~d~%" row col)
		(setf *gameBoard* valid)
	)

)


;------------------------------------------------------------------------------
; Function:     game-is-done
;
; Author:       Daniel Nix
;
; Description:  Checks if the game is over. Returns t if all spaces are filled.
;				Returns NIL otherwise
;
; Parameters:   none
;
; Return:       t if board is full, NIL otherwise
;------------------------------------------------------------------------------
(defun game-is-done ()
	(not (member '- *gameBoard*))
)


;------------------------------------------------------------------------------
; Function:     trade-turns
;
; Author:       Daniel Nix
;
; Description:  Checks if there are any valid moves for the next player. If
;				there are, it swaps the player and notifies who is next.
;				Otherwise, the current player is told to play again.
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun trade-turns ()

	(if (equal *color* 'b)

		(if (not (null (move-generator *gameBoard* 'w)))
			( setf *color* 'w)
			(format t "Sorry White, no moves for you!~%")
		)

		(if (not (null (move-generator *gameBoard* 'b)))
			( setf *color* 'b)
			(format t "Sorry Black, no moves for you!~%")
		)
	)
)

;------------------------------------------------------------------------------
; Function:     count-color
;
; Author:       Daniel Nix
;
; Description:  counts the number of pieces present on the board ()
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun count-color (color)
	
	(let 
		(
		(count '0)
		)

		(dolist (piece *gameBoard*)
			(if (equal piece color)
				(incf count)
			)
		)
		(return-from count-color count)
	)
)

;------------------------------------------------------------------------------
; Function:     print-game-stats
;
; Author:       Daniel Nix
;
; Description:  based on the final board state, shows pieces for each player
;				and the winner of this bout
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun print-game-stats ()

	(let ( (black_count '0) (white_count '0) )
		(setf black_count (count-color 'b))
		(setf white_count (count-color 'w))

		(format t "Black has ~d pieces~%" black_count)
		(format t "White has ~d pieces~%" white_count)
		(cond 
			( (= black_count white_count)
				(format t "Tie game!~%")
			)

			( (> black_count white_count)
			  (format t "Black is the winner!!!~%")
			)

			( t (format t "White is the winner!!!~%") )
		)
	)

)



