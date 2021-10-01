(defun getTileValue (tile)
	(+ (first tile) (getNth 2 tile) )
)

(defun isDouble (tile)
	(cond
		( (null tile)
			() )
		( t
			(equal (first tile) (first (rest tile) ) ) )
	)
)