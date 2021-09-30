(defun initGame ()
	(displayIntroMenu)
	(processIntroMenuChoice (getValidNumber 1 3 "Make your Selection") ) 
)

(defun displayIntroMenu ()
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

(defun resumeGame ()
	(playGame (loadSerializedGame (getValidNumber 1 3 "Choose a game (1) (2) or (3)") ) )
)

(defun loadSerializedGame (menuChoice)
	(cond
		( (= menuChoice 1)
			(append (serializedGame1) (list () ) ) )
		( (= menuChoice 2)
			(append (serializedGame2) (list () ) ) )
		( (= menuChoice 3)
			(append (serializedGame3) (list () ) ) )
	)
)

(defun newGame ()
	(princ "Starting New Game") (terpri)
	(playGame (startFirstRound) )
)

(defun playGame (game)
	(let(
		( scores (playRound game) ) )	
		(cond
			( (equal (getValidNumber 1 2 "Do you wish to keep playing? (1) Yes (2) No") 2)
				() )
			( t
				(playGame (nextRound scores game) ) )
		)
	)
)

(defun nextRound (scores game)
	;return a new round with 1 higher round number, empty trains, empty hands, empty boneyard
	;set players passed to false here as well, when thats implemented
	;Round arguments are in same order as serialization file, plus 1 more field to hold who has passed	
	;roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer passed
	(startRound (list (+ (getRoundNumber game) 1) (getNth 1 scores) () () (getNth 2 scores) () () () () () () ) )
)








