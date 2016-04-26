
(defun static (position player)
	(let 
		(
		numPlayer
		numOppose
		opposer
		)

		; Figure out what color the 
		(if (equal player 'b)
			(setf opposer 'w)
			(setf opposer 'b)
		)
		(setf numPlayer (count-color player))
		(setf numOppose (count-color opposer))

		; Return the number of player minus number of opposing pieces
		(- numPlayer numOppose)
	)
)