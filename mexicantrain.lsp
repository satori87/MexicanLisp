(defun mexicanTrain()
	(doLoads)
	(introMenu)
	(princ "Thank you for playing. Goodbye.")
)

(defun doLoads()
	(load "C:/lisp/boneyard.lsp")
	(load "C:/lisp/utility.lsp")
	(load "C:/lisp/round.lsp")
	(load "C:/lisp/serialize.lsp")
	(load "C:/lisp/deserialize.lsp")
)

(defun introMenu()
	(displayIntroMenu)
	(processIntroMenuChoice (getValidNumber 1 3 "Make your Selection") ) 
)

(defun displayIntroMenu()
	(terpri) (princ "Welcome to Mexican Train") (terpri)
	(princ "(1) Resume previous game") (terpri)
	(princ "(2) New game") (terpri)
	(princ "(3) Quit the game") (terpri)
)

(defun processIntroMenuChoice (menuChoice)
	(cond
		( (= menuChoice 1)
			(resumeGame) )
		( (= menuChoice 2)
			(newGame) )
		( (= menuChoice 3)
			nil )
	)
)

(defun resumeGame()
	(princ "Unimplemented") (terpri)
)

(defun newGame()
	(princ "Starting New Game") (terpri)
	;Round arguments are in same order as serialization file
	;roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer
	(playRound (startFirstRound) )
)

(mexicanTrain)