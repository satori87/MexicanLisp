(defun getRoundNumber (game)
	(getNth 1 game)
)

(defun getComputerScore (game)
	(getNth 2 game)
)

(defun getComputerHand (game)
	(getNth 3 game)
)

(defun getComputerTrain (game)
	(reverseList (getNth 4 game) )
)

(defun getHumanScore (game)
	(getNth 5 game)
)

(defun getHumanHand (game)
	(getNth 6 game)
)

(defun getHumanTrain (game)
	(getNth 7 game)
)

(defun getMexicanTrain (game)
	(getNth 8 game)
)

(defun getBoneyard (game)
	(getNth 9 game)
)

(defun getNextPlayer (game)
	(getNth 10 game)
)

(defun getComputerPassed (game)
	(getNth 1 (getNth 11 game) )
)

(defun getHumanPassed (game)
	(getNth 2 (getNth 11 game) )
)

(defun loadGame ()
	(princ "Enter a valid filename") (terpri)
	(let* (
		(input (read-line) )
		(ins (open (make-pathname :directory "lisp" :name input) ) )
		)
		(cond 
			( (null ins)
				(loadGame) )
			( t
				(append (read ins) (list () ) ) )
		)
	)
)



