(defun test-while ()
	(let ((curr-pos 10))
		(loop while (/= (mod curr-pos 8) 7 ) do
			(print curr-pos)
			(setf curr-pos (+ curr-pos -1) )
			(if (equal (nth curr-pos position) '-) ((setf check_pos curr-pos) return ))
		)
	)
)
