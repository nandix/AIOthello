;------------------------------------------------------------------------------
; Function:     printBoard
;
; Author:       Daniel Nix
;
; Description:  printBoard prints a formatted Othello board to the terminal
;				in an 8x8 grid
;
; Parameters:   boardList - the list representing the current board state
;
; Return:       none
;------------------------------------------------------------------------------
(defun printBoard ()

	(let 
		(
			(splitBoard (splitBoardList *gameBoard* '8))
			(rowIndex '1)
		)
		; Print the header to the terminal
		(format t "     1 2 3 4 5 6 7 8~%")

		; For each row
		(dolist (row splitBoard)
			; Print the row index
			(format t "   ~d " rowIndex)
			; For each element in that row
			(dolist (rowElem row)
				;Print the element
				(format t "~s " rowElem)
			)
			(format t "~%")

			; Increment the row we're printing
			(incf rowIndex)
		)
		(format t "~%")
	)
)

;------------------------------------------------------------------------------
; Function:     splitBoardList
;
; Author:       Daniel Nix
;
; Description:  splitBoardList accepts a list representing an Othello board 
;               and splits it into sublists of puzPerLine
;               elements each. This is done so no indexing, modulus, or other
;               calculations are required when printing the board.
;
; Parameters:   puzzleList - the list of puzzles which form the solution
;               puzPerLine - number of puzzles which should be printed
;                               on a single row
;
; Return:       splitList - the puzzle list with its entries split into
;                           puzPerLine puzzles in each sublist
;------------------------------------------------------------------------------
(defun splitBoardList (puzzleList puzPerLine)
    ; Compute the number of lines based on the puzzles per line
    (setf nLines (ceiling (/ (length puzzleList) puzPerLine)))
    ; Construct a list of NILs of length of number of lines
    (setf splitList (make-list nLines))


    ; For each row of n puzzles to print out
    (let ((rowIndex 0))
        ; For each puzzle to be printed...
        (dotimes (i (length puzzleList))
            ; Increment the rowIndex if the current line already 
            ;   had puzPerLine puzzles
            (cond 
                (
                    (and (> i '0) (= '0 (mod i puzPerLine))) 
                    (setf rowIndex (1+ rowIndex)) 
                )
                
                (t NIL)
            )
            ;Append the current puzzle to the appropriate index in the splitList
            (setf (nth rowIndex splitList) 
                (append (nth rowIndex splitList) (list  (nth i puzzleList)) )
            )
            
        )

        ; Return the list of puzzles split into puz per line
        (return-from splitBoardList splitList)
    )
)