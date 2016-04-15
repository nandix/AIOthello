
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

	(printBoard 'NIL)

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





