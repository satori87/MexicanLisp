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
			(serializedGame1) )
		( (= menuChoice 2)
			(serializedGame2) )
		( (= menuChoice 3)
			(serializedGame3) )
	)
)

(defun askToKeepPlaying (game)
	(processKeepPlayingPrompt game (getValidNumber 1 2 "Do you wish to keep playing? (1) Yes (2) No") )
)

(defun processKeepPlayingPrompt (game choice)
	(cond
		( (equal choice 1)
			(playRound (startRound (setRoundNumber game (+ (getRoundNumber game) 1) ) ) ) )
		( (equal choice 2)
			(quit) )
	)
)

(defun newGame ()
	(princ "Starting New Game") (terpri)
	(playGame (startFirstRound) )
)

(defun playGame (game)
	(playRound game)
	(cond
		( (equal (askToKeepPlaying) 2)
			(quit) )
		( t
			(playGame (nextRound game) ) )
	)
)

(defun nextRound (game)
	;return a new round with 1 higher round number, empty trains, empty hands, empty boneyard
	;set players passed to false here as well, when thats implemented
	;Round arguments are in same order as serialization file	
	;roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer
	(startRound (list (+ (getRoundNumber game) 1) (getComputerScore game) () () (getHumanScore game) () () () () () ) )
)








