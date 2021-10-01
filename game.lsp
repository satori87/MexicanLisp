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
	(playGame (loadGame) )
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
	(playGame (startRound (list 1 0 () () 0 () () () () () () ) ) )
)

(defun playGame (game)
	(let(
		( scores (tallyScores (playRound game) ) ) )
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


(defun tallyScores (game)
	(let* (
		(computerPts (tallyHand (getComputerHand game) ) )
		(humanPts (tallyHand (getHumanHand game) ) )
		(computerScore (+ (getComputerScore game) computerPts) )
		(humanScore (+ (getHumanScore game) humanPts) ))

		(format t "Computer adds ~d to its score for a total of ~d" computerPts computerScore) (terpri)
		(format t "Human adds ~d to its score for a total of ~d" humanPts humanScore) (terpri)

		;return the scores back to endRound, back through every round that was played
		;and back to playGame, which handles the rest
		(list computerScore humanScore)
	)
)