;------------------------------------------------------------------------------
; Function:     static
;
; Author:       Steph Athow, Dan Nix
;
; Description:  Computes a static evaluation function for a given othello board.
;				Begins with the difference between one player and another and
;				adds to it:
;					- corners worth +5 (corners cannot be flipped)
;					- edges worth +2 (edges less likely to be flipped)
;				It is an attempt to give "good" board positions a high score
;				and "bad" board positions a lower score.
;
; Parameters:   position: current board state
;				player: the player we are evaluating for ('b or 'w)
;
; Return:       total_val: the total accumulated value from the SEF
;------------------------------------------------------------------------------
(defun static (position player)
	(let 
		(
			numPlayer
			numOppose
			opposer
			total_val
		)

		; Figure out what color the 
		(if (equal player 'b)
			(setf opposer 'w)
			(setf opposer 'b)
		)
		(setf numPlayer (count-color player))
		(setf numOppose (count-color opposer))

		; Baseline: number of player minus number of opposing pieces
		(setf total_val (- numPlayer numOppose))

		; Controlling a corner is worth 5
		(when (not (equal (nth 0 position) '-)) (setf total_val (+ total_val 5 )) )
		(when (not (equal (nth 7 position) '-)) (setf total_val (+ total_val 5 )) )
		(when (not (equal (nth 54 position) '-)) (setf total_val (+ total_val 5 )) )
		(when (not (equal (nth 63 position) '-)) (setf total_val (+ total_val 5 )) )

		; Controlling an edge is worth 2 per piece
		(dotimes (i 64)
			; left edge
			(when (and (equal (mod i 8) 0) 
					   (equal (nth i position) player) 
				  )
				(setf total_val (+ total_val 1))
			)
			; right edge
			(when (and (equal (mod i 8) 7) 
					   (equal (nth i position) player) 
				  )
				(setf total_val (+ total_val 1))
			)
			; top or bottom edge
			(when (or (< i 8) (> i 53) )
				(setf total_val (+ total_val 1))
			)
		)

	total_val ; return sef value
	); end let
)
