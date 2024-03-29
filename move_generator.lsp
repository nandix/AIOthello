;------------------------------------------------------------------------------
; ********************* Move_Generator.lsp ***********************************
;
;Function: move-generator 
;Author: Stephanie Athow
;
;Description: Generates possible successor moves given current board state
;
;Parameters: 
;	Position: a list containing the current board state
;	Player: the player making a move, will be black ('b) or white ('w)
;
;Returns: a list of available moves from the current board state
;
;Notes:
;	Moves Rules:
;		-Capture at least 1 of opposing color
;		-Placed adjacent to opposing color (left, right, up, down, and diagonal)
;		-New piece must be in line (left, right, up, down, diagonal)
;			with an exisiting board piece 
;		-Pieces are flipped between placed piece and exisiting piece of 
;			player's color. Also check for pieces to be flipped that 
;			connect to other pieces of player's color
;------------------------------------------------------------------------------
(defun move-generator (position player)
	(do*
		(
			; position of player's piece
			(play_pos 0 (setf play_pos (+ 1 play_pos) ) )
			(check_pos 0)

			; successors lists
			(successors nil)
			(loop_successors nil)
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
		((= play_pos 64) successors)

		; set oppenent color
;		(if (equal player 'B) (setf oppose 'W) (setf oppose 'B) )

		; perform check and traverse until:
			; hit own color - do nothing & move on
			; hit wall - do nothing & move on
			; hit empty space - place move & flip pieces

		; check left
		(setf check_pos (- play_pos 1) )
		(when (and (> play_pos 0)
			(equal (nth play_pos position) player)
			(not (equal (nth check_pos position) player))
			(not (equal (nth check_pos position) '-))
			(> (mod play_pos 8) 0) )

			; search for edge of board or until blank space has been found
			(loop while (/= (mod check_pos 8) 7) do
				(when (equal (nth check_pos position) '-) 
					(setf left (board-generate position check_pos player)) 
					(return)
				)
				; if player piece has been encountered, stop looking
				(when (equal (nth check_pos position) player)
					(return)
				)
				(setf check_pos (+ check_pos -1) ) 
			)
		) 

		; check right
		(setf check_pos (+ play_pos 1) )
		(when (and (< play_pos 63)
			(equal (nth play_pos position) player)
			(not (equal (nth check_pos position) player))
			(not (equal (nth check_pos position) '-))
			(< (mod play_pos 8) 7) )

			; search for edge of board or until blank space has been found
			(loop while (/= (mod check_pos 8) 0) do
				(when (equal (nth check_pos position) '-) 
					(setf right (board-generate position check_pos player)) 
					(return)
				)
				; if player piece has been encountered, stop looking
				(when (equal (nth check_pos position) player)
					(return)
				)
				(setf check_pos (+ check_pos 1) ) 
			)
		)

		; check up
		(setf check_pos (- play_pos 8) )
		(when (and (> play_pos 7)
			(equal (nth play_pos position) player)
			(not (equal (nth check_pos position) player))
			(not (equal (nth check_pos position) '-))
			)

			; search for edge of board or until blank space has been found
			(loop while (> check_pos -1) do
				(when (equal (nth check_pos position) '-) 
					(setf up (board-generate position check_pos player) ) 
					(return)
				)
				; if player piece has been encountered, stop looking
				(when (equal (nth check_pos position) player)
					(return)
				)

				(setf check_pos (+ check_pos -8) ) 
			)
		)

		; check down 
		(setf check_pos (+ play_pos 8) )
		(when (and (< play_pos 56)
			(equal (nth play_pos position) player)
			(not (equal (nth check_pos position) player))
			(not (equal (nth check_pos position) '-))
			)

			; search for edge of board or until blank space has been found
			(loop while (< check_pos 64) do
				(when (equal (nth check_pos position) '-) 
					(setf down (board-generate position check_pos player) ) 
					(return)
				)
				; if player piece has been encountered, stop looking
				(when (equal (nth check_pos position) player)
					(return)
				)

				(setf check_pos (+ check_pos 8) ) 
			)
		)

		; check up-left 
		(setf check_pos (- play_pos 9) )
		(when (and (> play_pos 8) 
			(> (mod play_pos 8) 0) 
			(equal (nth play_pos position) player)
			(not (equal (nth check_pos position) player))
			(not (equal (nth check_pos position) '-))
			)

			; search for edge of board or until blank space has been found
			(loop while ( and  (> check_pos -1) (/= (mod check_pos 8) 7)) do
				(when (equal (nth check_pos position) '-) 
					(setf up-left (board-generate position check_pos player) ) 
					(return)
				)
				; if player piece has been encountered, stop looking
				(when (equal (nth check_pos position) player)
					(return)
				)
				(setf check_pos (+ check_pos -9) ) 
			)
		)

		; check up-right
		(setf check_pos (- play_pos 7) )
		(when (and (> play_pos 8) 
			(< (mod play_pos 8) 8) 
			(equal (nth play_pos position) player)
			(not (equal (nth check_pos position) player))
			(not (equal (nth check_pos position) '-))
			)

			; search for edge of board or until blank space has been found
			(loop while ( and (> check_pos 0) (/= (mod check_pos 8) 0)) do
				(when (equal (nth check_pos position) '-) 
					(setf up-right (board-generate position check_pos player) ) 
					(return)
				)
				; if player piece has been encountered, stop looking
				(when (equal (nth check_pos position) player)
					(return)
				)
				(setf check_pos (+ check_pos -7) ) 
			)
		)

		; check down-left
		(setf check_pos (+ play_pos 7) )
		(when (and (< play_pos 56)
			(> (mod play_pos 8) 0)
			(equal (nth play_pos position) player)
			(not (equal (nth check_pos position) player))
			(not (equal (nth check_pos position) '-))
			)

			; search for edge of board or until blank space has been found
			(loop while ( and (< check_pos 63) (/= (mod check_pos 8) 7)) do
				(when (equal (nth check_pos position) '-) 
					(setf down-left (board-generate position check_pos player) ) 
					(return)
				)
				; if player piece has been encountered, stop looking
				(when (equal (nth check_pos position) player)
					(return)
				)
				(setf check_pos (+ check_pos 7) ) 
			)
		)

		; check down-right
		(setf check_pos (+ play_pos 9) )
		(when (and (< play_pos 55)
			(< (mod play_pos 8) 8)
			(equal (nth play_pos position) player)
			(not (equal (nth check_pos position) player))
			(not (equal (nth check_pos position) '-))
			)

			; search for edge of board or until blank space has been found
			(loop while ( and (< check_pos 64) (/= (mod check_pos 8) 0)) do
				(when (equal (nth check_pos position) '-) 
					(setf down-right (board-generate position check_pos player) ) 
					(return)
				)
				; if player piece has been encountered, stop looking
				(when (equal (nth check_pos position) player)
					(return)
				)

				(setf check_pos (+ check_pos 9) ) 
			)
		)

		(setf successors (list* left right up down up-left up-right down-left down-right successors) )
		(setf successors (remove nil successors))

		(setf loop_successors nil)
		(setf left nil)
		(setf right nil)
		(setf down nil)
		(setf up nil)
		(setf up-left nil)
		(setf up-right nil)
		(setf down-left nil)
		(setf down-right nil)

	) ;end do*
); end move-generate

;------------------------------------------------------------------------------
;Function: board-generate
;
;Author: Stephanie Athow
;
;Description: Generates successor move based on current board state
;
;Parameters: 
;	Position: a list containing the current board state
;	move_pos: the position to place the current player's piece
;	Player: the player making a move, will be black ('b) or white ('w)
;
;Returns: a list of available moves from the current board state
;------------------------------------------------------------------------------
(defun board-generate (position placed_pos player)
	(let (
		(new_board (copy-list position)) 	; copy current board
		(check_pos 0)			; position on the board to check pieces to flip
		(check_dir 0)			; direction to check for pieces to flip
		(flipped_flag nil)
		(flip nil) )		; flag we want to flip, holds position to start flipping

		; set oppenent color
		(if (equal player 'B) (setf oppose 'W) (setf oppose 'B) )

		; Check if we tried to place a piece in a space already occupied
		(if (equal (nth placed_pos new_board) '-)
			(setf (nth placed_pos new_board) player)
			(return-from board-generate new_board)
		)	

		; ********** check left of placed piece ********** 
		(setf check_dir -1 )
		(setf flip (check-flip-pieces position placed_pos player check_dir) )
		
		; if not nil, then flip
		(if flip 	(loop while (/= flip placed_pos ) do
					(setf (nth flip new_board) player)
					(setf flip (- flip check_dir) )
					(setf flipped_flag 1)
					) ; end loop
		) ;end if, (do nothing if not true)


		; ********** check right of placed piece ********** 
		(setf check_dir 1)
		(setf flip (check-flip-pieces position placed_pos player check_dir) )

		; if not nil, then flip
		(if flip 	(loop while (/= flip placed_pos ) do
					(setf (nth flip new_board) player)
					(setf flip (- flip check_dir) )
					(setf flipped_flag 1)
					) ; end loop
		) ;end if, (do nothing if not true)


		; ********** check up of placed piece ********** 
		(setf check_dir -8 )
		(setf flip (check-flip-pieces position placed_pos player check_dir) )
	
		; if not nil, then flip
		(if flip 	(loop while (/= flip placed_pos ) do
					(setf (nth flip new_board) player)
					(setf flip (- flip check_dir) )
					(setf flipped_flag 1)
					) ; end loop
		) ;end if, (do nothing if not true)


		; ********** check down of placed piece ********** 
		(setf check_dir 8)
		(setf flip (check-flip-pieces position placed_pos player check_dir) )
	
		; if not nil, then flip
		(if flip 	(loop while (/= flip placed_pos ) do
					(setf (nth flip new_board) player)
					(setf flip (- flip check_dir) )
					(setf flipped_flag 1)
					) ; end loop
		) ;end if, (do nothing if not true)


		; ********** check up-left of placed piece ********** 
		(setf check_dir -9 )
		(setf flip (check-flip-pieces position placed_pos player check_dir) )
	
		; if not nil, then flip
		(if flip 	(loop while (/= flip placed_pos ) do
					(setf (nth flip new_board) player)
					(setf flip (- flip check_dir) )
					(setf flipped_flag 1)
					) ; end loop
		) ;end if, (do nothing if not true)


		; ********** check up-right of placed piece ********** 
		(setf check_dir -7)
		(setf flip (check-flip-pieces position placed_pos player check_dir) )
	
		; if not nil, then flip
		(if flip 	(loop while (/= flip placed_pos ) do
					(setf (nth flip new_board) player)
					(setf flip (- flip check_dir) )
					(setf flipped_flag 1)
					) ; end loop
		) ;end if, (do nothing if not true)


		; ********** check down-left of placed piece ********** 
		(setf check_dir 7)
		(setf flip (check-flip-pieces position placed_pos player check_dir) )
	
		; if not nil, then flip
		(if flip 	(loop while (/= flip placed_pos ) do
					(setf (nth flip new_board) player)
					(setf flip (- flip check_dir) )
					(setf flipped_flag 1)
					) ; end loop
		) ;end if, (do nothing if not true)


		; ********** check down-right of placed piece ********** 
		(setf check_dir 9)
		(setf flip (check-flip-pieces position placed_pos player check_dir) )
	
		; if not nil, then flip
		(if flip 	(loop while (/= flip placed_pos ) do
					(setf (nth flip new_board) player)
					(setf flip (- flip check_dir) )
					(setf flipped_flag 1)
					) ; end loop
		) ;end if, (do nothing if not true)

	(if (not flipped_flag)
		(setf (nth placed_pos new_board) '-)
	)
	new_board
	); end let
) ;end board-generate


;------------------------------------------------------------------------------
;Function: check-flip-pieces
;
;Author: Stephanie Athow
;
;Description: searches a direction checking if the placed piece will	cause 
;	opponent pieces to flip.
;
;Parameters: 
;	position: a list containing the current board state
;	start_pos: start location of recursion
;	player: the player making a move, will be black ('b) or white ('w)
;	dir: direction to move searching for valid move
;
;Returns: position of valid move or nil
;------------------------------------------------------------------------------
(defun check-flip-pieces (position start_pos player dir)
	(let
		((check_pos) (prev_pos) (check_wrap)) 		; position to check for flipping

		(setf start_flip nil) 	; position to return to start flipping
		
		(setf check_pos (+ start_pos dir))
		(setf prev_pos (mod start_pos 8))

		; check for wrap around col 0 and/or 7.
		; direction negative - check for wrap back to col 7
		; direction positive - check from wrap forward to col 0 
		(if (< dir 0) (setf check_wrap 7) (setf check_wrap 0) )

		; set oppenent color
		(if (equal player 'B) (setf oppose 'W) (setf oppose 'B) )

		; search along direction until:
		; 	edge of board
		; 	blank found
		; 	piece of same color is found
		(loop while( and (> check_pos -1) 	; check start of board
					(< check_pos 64)	  	; check end of board
					(/= (abs (- (mod check_pos 8) prev_pos)) 7)
			  	    ) do ; end while conditions

			; check to see if it's a blank spot, if yes, return
			(when (equal (nth check_pos position) '-) 
				(return)
			)
			; check to see if there's a piece the same color as player
			; if piece is same as player color, flag we want to flip
			(when (equal (nth check_pos position) player)
				(setf start_flip (- check_pos dir) )
				(return)
			) ; end when

			(setf prev_pos (mod check_pos 8))

			; increment while loop counter
			(setf check_pos (+ check_pos dir) )

		) ; end while loop
		start_flip
	) ; end let
) ; end check-flip-pieces

;------------------------------------------------------------------------------
;Function: validate-move
;
;Author: Stephanie Athow
;
;Description: validates and returns the board state after the user places their piece
;
;Parameters: 
;	Position: a list containing the current board state
;	user_pos: the position to place the current player's piece
;	player: the player making a move, will be black ('b) or white ('w)
;
;Returns: the board state after the user placed their piece or nil if it was 
;	and invalid move.
;------------------------------------------------------------------------------
(defun validate-move (position user_pos player)
	(let (
		(new_board (copy-list position)) 	; copy current board
		(check_pos 0)			; position on the board to check pieces to flip
		(check_row 0)
		(check_col 0)
		)

		; convert user input to list variable
		; get user inputs: (row column)
		(setf check_row (nth 0 user_pos) )
		(setf check_col (nth 1 user_pos) )

		; account for zero indexing
		(setf check_row (+ check_row -1) )
		(setf check_col (+ check_col -1) )

		; convert to a position in our board state list
		(setf check_pos (+ (* check_row 8) check_col ) )

		(setf new_board (board-generate position check_pos player))

		; check if board is same
		(if (equal position new_board) (setf new_board nil) new_board)

		; return board-state
		new_board

	) ; end let
) ;end validate-move

