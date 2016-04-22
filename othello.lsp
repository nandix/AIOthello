
; Initialize globals
(defvar *gameBoard*)
(defvar *firstMove* 'NIL)
(defvar *color* 'black)

(load 'printBoard)
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
	(make-user-move)
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

	(let ((row 'NIL) (col 'NIL) (valid 'NIL))
		(loop while (not valid)

			(format t "What is your move [row col]? ")
			(setf row (read))
			(setf col (read))
			(setf valid (validate-move *gameBoard* (list row col) *color*) )
			(if (not valid)
				(format t "Invalid move, please try again~%")
			)
		)
		(format "Row is: ~d, col: ~d~%" row col)
		(setf *gameBoard* valid)
	)

)




