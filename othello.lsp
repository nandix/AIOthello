;------------------------------------------------------------------------------
; Program:		Othello
;
; Authors:      Steph Athow, Daniel Nix
;
; Course: 	Artificial Intellegence
;
; Professor: Dr. Weiss
;
; Description:  (From Specificaiton Document) Othello (also known as Reversi) 
;			is played on an 8x8 grid with 64 stones that are black on one
;			side and white on the other. 
;
;			This program uses Minimax with Alpha Beta pruning to serve as a
;			computer player. The game may be started in two ways:
;				1) clisp othello.lsp [black|white]
;				2) clisp
;					From inside interpreter: 	(load 'othello)
;												(othello '[black|white])
;
;			First, the user decides between Human vs Human, Human vs Computer,
;			and Computer vs computer.
;
;			A startup sequence to choose who goes first follows.
;			At this point, the user is also prompted to choose how deep into
;			the minimax tree the search should go (the number of plys)
;
;			If the user color wins, the game is over. If not, a prompt for
;			revenge is provided
;
; Parameters:   player: the color to play as (either "black" or "white", 
;						not b or w)
;
; Return:       none
;------------------------------------------------------------------------------


; Initialize globals used for tracking colors, the single board state, and
;	whose turn it is
(defvar *gameBoard*)
(defvar *firstMove* 'NIL)
(defvar *playerColor* 'black)
(defvar *oppColor* 'white)
(defvar *curColor* 'black)
(defvar *playMode* 0)
(defvar *playerPlys* 1)
(defvar *oppPlys* 1)

(load "printBoard.lsp")
(load "move_generator.lsp")
(load "static_eval.lsp")
(load "make_move.lsp")
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
; Parameters:   player: the color of pieces the user player wants to be
;
; Return:       none
;------------------------------------------------------------------------------
(defun othello ( player )

	; Ge the color from input arg
	(setf *playerColor* player)
	;If the color is black, set it to b. Otherwise, set to w.
	(if (or (equal *playerColor* 'black) (equal *playerColor* "black"))
		(setf *playerColor* 'b)
		(setf *playerColor* 'w)
	)
	; Set opposition player based on user player color
	(if (equal *playerColor* 'b)
		(setf *oppColor* 'w)
		(setf *oppColor* 'b)
	)

	; Set up 2 player, H v C, or C v C. Also determine who goes first
	(othello-set-play-style)
	
	(let ((keep_playing 't))
		(loop while (not (null keep_playing)) do
			

			; Initialize the game board
			(othello-init)

			; Print the start state for the user
			(printBoard)

			; While the game is not over
			(loop while (not (game-over)) do
				; Take a turn
				(take-turn)
				; Print the board
				(printBoard)
				; Trade turns, if we haven't ended the game and moves
				;	are available to the next player
				(if (not (game-over)) 
					(trade-turns)
				)
			)

			; See if the user player won. If not, ask for revenge
			(let (winner)
				(setf winner (print-game-stats))

				; If the first player didn't win...
				(if (not (equal winner *playerColor*))
					(setf keep_playing (ask-for-revenge))
					(setf keep_playing 'NIL)
				)
			)
		)
	)


)


;------------------------------------------------------------------------------
; Function:     ask-for-revenge
;
; Author:       Daniel Nix
;
; Description:  If the player loses, ask if the player wants revenge
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun ask-for-revenge ()
	; 
	(format t "  You lost! Would you like a chance at revenge [y/n]? ")
	(let (revenge? 'NIL)
		(loop while (null revenge?) do
			(setf revenge? (read))
			(cond
				((equal revenge? 'y)
					(return-from ask-for-revenge 't)
				)
				((equal revenge? 'n)
					(return-from ask-for-revenge 'NIL)
				)
				(t 
					(format t "Invalid entry. Please enter y or n: ")
					(setf revenge? 'NIL)
				)
			)
		)
	)
)

;------------------------------------------------------------------------------
; Function:     othello-set-play-style
;
; Author:       Daniel Nix
;
; Description:  othello-set-play-style is responsible for configuring the 
;
; Parameters:   none
;
; Return:       none
;------------------------------------------------------------------------------
(defun othello-set-play-style ()

	; Determine which style game the user wants to play
	(format t "How would you like to play:~%")
	(format t "  (1) Human vs Human~%")
	(format t "  (2) Human vs Computer~%")
	(format t "  (3) Computer vs Computer~%")
	(format t "Please enter selection: ")
	; While the user has entered an invalid option, prompt again
	(loop while (or (< *playMode* 1) (> *playMode* 3)) do
		(setf *playMode* (read))
		(if (or (< *playMode* 1) (> *playMode* 3))
			(format t "Invalid mode. Please try again: ")
		)
	)
	

	; Ask the user if they would like to move first
	(cond 
		((< *playMode* 3)
			(format t "Would you like to move first [y/n]? ")
			(setf *firstMove* (read))

			; For simplicity later, set first move to t or nil,
			;	not y or n
			(cond 
				((equal *firstMove* 'y)
					(setf *firstMove* 't)
				)
				(t
					(setf *firstMove* 'NIL)
				)
			)
			
		)
	)

	; If the user wants to play againsta  computer, or pit two computers against
	;	each other, get the plys to search here
	(cond 
		((= *playMode* 2)
			(format t "Enter number of plys to search: ")
			(setf *oppPlys* (read))
		)

		((= *playMode* 3)
			(format t "Enter number of plys for ~s: " *playerColor*)
			(setf *playerPlies* (read))
			(format t "Enter number of plys for ~s: " *oppColor*)
			(setf *oppPlys* (read))
		)

	)


	(format t "OK! You will be playing as ~s. When asked for your move, please enter the row~%
	and column in which you would like to place a ~s stone. Remember, you must~%
	outflank at least one ~s stone, or forfeit your move.~%~%" *playerColor* *playerColor* *oppColor*)
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

	; Initialize the starting position
	(setf *gameBoard* 
		'( 	- - - - - - - -
			- - - - - - - -
			- - - - - - - -
			- - - W B - - -
			- - - B W - - -
			- - - - - - - -
			- - - - - - - -
			- - - - - - - - ))


	; To start the game, set the first player to black
	(setf *curColor* 'b)
	
)

;------------------------------------------------------------------------------
; Function:     make-user-move
;
; Author:       Daniel Nix
;
; Description:  Prompts a user for a row/col for the next move. Loops until
;				a valid move has been made. The validity of move is determined by
;				the validate-move function.
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
			(setf valid (validate-move *gameBoard* (list row col) *curColor*) )
			(if (not valid)
				(format t "Invalid move, please try again~%")
			)
		)
		(format t "~%")
		(setf *gameBoard* valid)
	)

)

;------------------------------------------------------------------------------
; Function:     make-computer-move
;
; Author:       Daniel Nix
;
; Description:  Makes a move for a computer player. This serves as a layer
;				over the make-move function so that user and computer moves
;				follow the same form
;
; Parameters:   none
;
; Return:       t if board is full, NIL otherwise
;------------------------------------------------------------------------------
(defun make-computer-move (board player numPlys)

	(let (move index)
		; Use the required make-move function to determine the position
		(setf move (make-move board player numPlys))

		(format t "Computer ~s moving to (row,col): (~d, ~d)~%~%" 
			player (nth 0 move) (nth 1 move))

		; Make the move from the make-move function
		(setf *gameBoard* (validate-move *gameBoard* move player))


	)

)


;------------------------------------------------------------------------------
; Function:     game-over
;
; Author:       Daniel Nix
;
; Description:  Checks if the game is over. Returns t if all spaces are filled
;				or no valid moves are left. Returns NIL otherwise
;
; Parameters:   none
;
; Return:       t if board is game has finished, NIL otherwise
;------------------------------------------------------------------------------
(defun game-over ()
	; The game is over when there are either...
	(or
		; No more spaces
		(not (member '- *gameBoard*))
		; Neither player can move
		(and
			(null (move-generator *gameBoard* 'w))
			(null (move-generator *gameBoard* 'b))
		)
	)
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

	(if (equal *curColor* 'b)

		(if (not (null (move-generator *gameBoard* 'w)))
			( setf *curColor* 'w)
			(format t "Sorry White, no moves for you!~%")
		)

		(if (not (null (move-generator *gameBoard* 'b)))
			( setf *curColor* 'b)
			(format t "Sorry Black, no moves for you!~%")
		)
	)
)

;------------------------------------------------------------------------------
; Function:     count-color
;
; Author:       Daniel Nix
;
; Description:  counts the number of pieces of a given color present on the board 
;
; Parameters:   color: 'b or 'w to count
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
		; Count how many pieces from each side
		(setf black_count (count-color 'b))
		(setf white_count (count-color 'w))

		; Print how many pieces on each side
		(format t "Black has ~d pieces~%" black_count)
		(format t "White has ~d pieces~%" white_count)
		(cond 
			; If it's a tie game, print stats and return the opposing
			;	color
			( (= black_count white_count)
				(format t "Tie game!~%")
				(return-from print-game-stats *oppColor*)
			)
			; If black won, print stats and return black
			( (> black_count white_count)
			 	(format t "Black is the winner!!!~%")
			  	(return-from print-game-stats 'b)
			)
			; If white won, print stats and return white
			( t 
				(format t "White is the winner!!!~%") 
				(return-from print-game-stats 'w)
			)

		)
	)

)

;------------------------------------------------------------------------------
; Function:     take-turn
;
; Author:       Daniel Nix
;
; Description:  Depending on who the current player is, take the next move 
;				from the user or computer
;
; Parameters:   Optional- numPlys: plys to search for minimax
;
; Return:       none
;------------------------------------------------------------------------------
(defun take-turn ( &optional (numPlys 1) )

	; If human vs human, call the human move
	(cond  
		((= *playMode* 1)
			(format t "Player ~s, your turn!~%" *curColor*)
			(make-user-move)
		)
	)

	; If human vs computer, call human if player turn. Otherwise,
	;	call computer
	(if (= *playMode* 2)
		(cond 
			((equal *curColor* *playerColor*)
				(format t "Player ~s, your turn!~%" *curColor*)
				(make-user-move)
			)
			(t
				;(format t "Computer turn as ~s~%" *curColor*)
				(make-computer-move *gameBoard* *oppColor* *oppPlys*)
			)
		)
	)

	; If human vs computer, call computer then computer
	(cond 
		((= *playMode* 3)
			;(format t "Computer turn as ~s~%" *curColor*)
			(if (equal *curColor* *playerColor*)
				(make-computer-move *gameBoard* *curColor* *playerPlies*)
				(make-computer-move *gameBoard* *curColor* *oppPlys*)
			)
		)
	)
)

; If run from the command line, start it here!
;If the user supplied a cmdline argument, call 8puzzle with that argument
; (cond
; 	( (> (length *args*) 0)
; 		(othello  (car *args*))
; 	)
; )

