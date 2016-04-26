(defun make-move (boardState player ply)

	(let (movedBoard) 
		(setf movedBoard 
			(car (nth 1 (minimax *gameBoard* ply '0 player 'MAX -1000000 1000000) ) ) )
		(setf *gameBoard* movedBoard)
	)
)
