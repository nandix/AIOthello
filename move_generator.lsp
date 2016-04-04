#|
			***** Move_Generator.lsp *****
Function: move-generator 
Author: Stephanie Athow

Description: Generates possible successor moves given current board state

Parameters: 
	Position: a list containing the current board state
	Player: the player making a move, will be black ('b) or white ('w)

Returns: a list of available moves from the current board state

Notes:
	Moves Rules:
		-Capture at least 1 of opposing color
		-Placed adjacent to opposing color (left, right, up, down, and diagonal)
		-New piece must be in line (left, right, up, down, diagonal)
			with an exisiting board piece 
		-Pieces are flipped between placed piece and exisiting piece of 
			player's color. Also check for pieces to be flipped that 
			connect to other pieces of player's color
**************************************************************************** |#

(setf test-position '(- - - - - - - - - - - - - - - - - - - - - - - - - - - w b - - - - - - b w - - - - - - - - - - - - - - - - - - - - - - - - - - -))

(setf test-player 'b)

(defun move-generator (position player)
	(do*
		(
			; position of player's piece
			(play_pos 0 (setf play_pos (+ 1 play_pos) ) )
			(check_pos 0)

			; successors lists
			(successors nil)
			(left nil)
			(right nil)
			(down nil)
			(up nil)
			(up-left nil)
			(up-right nil)
			(down-left nil)
			(down-right nil)
		) ; end set local vars

		; termination condition - all positions have been checked
		((= play_pos 64))

		; set oppenent color
		(if (equal player 'B) (setf oppose 'W) (setf oppose 'B) )

;		(print 'play_pos )
		(print play_pos)

		; perform check and traverse until:
			; hit own color - do nothing & move on
			; hit wall - do nothing & move on
			; hit empty space - place move & flip pieces

		; check left
		(setf check_pos (- play_pos 1) )
		(when (and (> play_pos 0)
			(equal (nth play_pos position) player)
			(equal (nth check_pos position) oppose)
			(> (mod play_pos 8) 0) )

			(setf left (board-generate position check_pos player) ) 

			(print 'left)
		) 

		; check right
		(setf check_pos (+ play_pos 1) )
		(when (and (< play_pos 64)
			(equal (nth play_pos position) player)
			(equal (nth check_pos position) oppose)
			(< (mod play_pos 8) 7) )
			(setf right (board-generate position check_pos player) )  

			(print 'right)
		)

		; check up
		(setf check_pos (- play_pos 8) )
		(when (and (> play_pos 7)
			(equal (nth play_pos position) player)
			(equal (nth check_pos position) oppose) )

			(setf up (board-generate position check_pos player) ) 

			(print 'up)
		)

		; check down 
		(setf check_pos (+ play_pos 8) )
		(when (and (< play_pos 56)
			(equal (nth play_pos position) player)
			(equal (nth check_pos position) oppose) )

			(setf down (board-generate position check_pos player) ) 

			(print 'down)
		)

		; check up-left 
		(setf check_pos (- play_pos 9) )
		(when (and (> play_pos 8) 
			(> (mod play_pos 8) 0) 
			(equal (nth play_pos position) player)
			(equal (nth check_pos position) oppose) )

			(setf up-left (board-generate position check_pos player) ) 

			(print 'up-left)

		)

		; check up-right
		(setf check_pos (- play_pos 7) )
		(when (and (> play_pos 8) 
			(< (mod play_pos 8) 8) 
			(equal (nth play_pos position) player)
			(equal (nth check_pos position) oppose) )

			(setf up-right (board-generate position check_pos player) ) 

			(print 'up-right)

		)

		; check down-left
		(setf check_pos (+ play_pos 7) )
		(when (and (< play_pos 56)
			(> (mod play_pos 8) 0)
			(equal (nth play_pos position) player)
			(equal (nth check_pos position) oppose) )

			(setf down-left (board-generate position check_pos player) ) 

			(print 'down-left)
		)

		; check down-right
		(setf check_pos (+ play_pos 9) )
		(when (and (< play_pos 56)
			(< (mod play_pos 8) 8)
			(equal (nth play_pos position) player)
			(equal (nth check_pos position) oppose) )

			(setf down-right (board-generate position check_pos player) ) 

			(print 'down-right)
		)

		(setf successors (list left right up down up-left up-right down-left down-right))

		(setf successors (remove nil successors))

		successors ; return successors
		(print successors)

	) ;end do*
); end move-generate

#| ****************************************************************************
Function: board-generate

Author: Stephanie Athow

Description: Generates successor move based on current board state

Parameters: 
	Position: a list containing the current board state
	move_pos: the position to place the current player's piece
	Player: the player making a move, will be black ('b) or white ('w)

Returns: a list of available moves from the current board state

***************************************************************************** |# 
(defun board-generate (position move_pos player)
	(let
		(new-board)
		(setq new-board (copy-list position)) 
		(setf check_pos 0)

		(setf (nth move_pos new-board) player)	

		; check left
		(setf check_pos (- move_pos 1) )
		(when (and (> check_pos 0)
			(equal (nth check_pos position) oppose)
			(> (mod check_pos 8) 0) )

			(setf (nth move_pos new-board) player)
			(setf (nth check_pos new-board) player)
		) 

		; check right
		(setf check_pos (+ move_pos 1) )
		(when (and (< check_pos 64)
			(equal (nth check_pos position) oppose)
			(< (mod check_pos 8) 7) )

			(setf (nth move_pos new-board) player)
			(setf (nth check_pos new-board) player)
		)

		; check up
		(setf check_pos (- move_pos 8) )
		(when (and (> check_pos 7)
			(equal (nth check_pos position) oppose) )

			(setf (nth move_pos new-board) player)
			(setf (nth check_pos new-board) player)
		)

		; check down 
		(setf check_pos (+ move_pos 8) )
		(when (and (< check_pos 56)
			(equal (nth check_pos position) oppose) )

			(setf (nth move_pos new-board) player)
			(setf (nth check_pos new-board) player)
		)

		; check up-left 
		(setf check_pos (- move_pos 9) )
		(when (and (> check_pos 8) 
			(> (mod check_pos 8) 0)
			(equal (nth check_pos position) oppose) )

			(setf (nth move_pos new-board) player)
			(setf (nth check_pos new-board) player)
		)

		; check up-right
		(setf check_pos (- move_pos 7) )
		(when (and (> check_pos 8) 
			(< (mod check_pos 8) 8)
			(equal (nth check_pos position) oppose) )

			(setf (nth move_pos new-board) player)
			(setf (nth check_pos new-board) player)
		)

		; check down-left
		(setf check_pos (+ move_pos 7) )
		(when (and (< check_pos 56)
			(> (mod check_pos 8) 0)
			(equal (nth check_pos position) oppose) )

			(setf (nth move_pos new-board) player)
			(setf (nth check_pos new-board) player)
		)

		; check down-right
		(setf check_pos (+ move_pos 9) )
		(when (and (< check_pos 56)
			(< (mod check_pos 8) 8)
			(equal (nth check_pos position) oppose) )

			(setf (nth move_pos new-board) player)
			(setf (nth check_pos new-board) player)
		)

	new-board
	); end let
) ;end board-generate



