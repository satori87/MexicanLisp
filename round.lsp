(defun startFirstRound ()
	(startRound (list 1 0 () () 0 () () () () () ) )
)

(defun startRound (game)
	(cond
		( (null (getBoneyard game) )
			(startRound (setBoneyard game (newBoneyard) ) ) )
		( (< (getListLength (getComputerHand game) ) 16)
			(startRound (setBoneyard (setComputerHand game (addTileToHand '"Computer" (getComputerHand game) (getBoneyard game) ) ) (rest (getBoneyard game) ) ) ) )
		( (< (getListLength (getHumanHand game) ) 16)
			(startRound (setBoneyard (setHumanHand game (addTileToHand '"Human" (getHumanHand game) (getBoneyard game) ) ) (rest (getBoneyard game) ) ) ) )
		( (null (getNextPlayer game) )
			(startRound  (setNextPlayer game (coinToss) ) ) )
		( t
			(format t '"Starting Round ~d" (getRoundNumber game) ) (terpri)
			game )
	)
)

(defun playRound (game)
	(printRound game)
	(cond 
		( (null (getHumanHand game) )
			(playRound (endRound game) ) )
		( (null (getComputerHand game) )
			(playRound (endRound game) )
		;( () () ) ;Pass/boneyard based victory condition!
		( (equal (getNextPlayer game) "Computer")
			(playRound (takeComputerTurn game) )
		( (equal (getNextPlayer game) "Human")
			(playRound (takeHumanTurn game) )
	)

)

(defun endRound (game)
	(princ "Round ~d over! Tallying scores" (getRoundNumber game) ) 
	(printEndOfRound (tallyScores game) )
)

(defun printEndOfRound (game)
	(princ "END END END") (terpri)
	(printRound game)
)

(defun tallyScores (game)
	(let (
		(computerHand (getComputerHand game) )
		(humanHand (getHumanHand game) ) )
		(cond
			( (equal (null computerHand ) nil )
				(tallyScores 
					(setComputerHand
						(setComputerScore game
						(+ (getComputerScore game) (getTileValue (first computerHand ) ) ) )
					(rest computerHand ) )
				)
			)
			( t
				game )
		)
	)
)

(defun printRound (game)
	(format t "Round ~d | Computer Score: ~d | Human Score: ~d" (getRoundNumber game) (getComputerScore game) (getHumanScore game) ) (terpri)
	(printListLn '"Computer Hand  : " (getComputerHand game) ) 
	(printListLn '"Human Hand     : " (getHumanHand game) )

	;Consider asking professor if its ok to just keep them separate
	;(printPlayerTrains (getComputerTrain game) (getEngine (getRoundNumber game) ) (getHumanTrain game) )
	(printSeparatePlayerTrains (getComputerTrain game) (getEngine (getRoundNumber game) ) (getHumanTrain game) )
	
	(printListLn '"Mexican Train  : " (getMexicanTrain game) )
	(format t   '"Boneyard (~2,'0d)  :" (getListLength (getBoneyard game) ) )
	(printTop (getBoneyard game) )
	(printTurn (getNextPlayer game) )
)

(defun printPlayerTrains (computerTrain engine humanTrain)
	(printList '"Player Trains  :" (reverseList computerTrain) )
	(format t '" ~d "  engine)
	(printListLn '"" humanTrain)
)

(defun printSeparatePlayerTrains (computerTrain engine humanTrain)
	(printListLn '"Computer Train : " computerTrain )
	(printListLn '"Human Train    : " humanTrain )
)

(defun getEngine (roundNumber)
	(let (
		( engineHalf (- 9 (- (mod roundNumber 10) 1)) ) )
		(list engineHalf engineHalf)
	)
)
;
(defun printTurn(nextPlayer)
	(format t '"It is ~d's turn" nextPlayer) (terpri)
)

(defun coinToss ()
	(princ "===========================================") (terpri)
	(getValidNumber 1 2 "Human player call (1) Heads or (2) Tails")
	(cond
		( (= (random 2) 0)
			(princ "Computer wins Coin Toss!") (terpri)
			"Computer" )
		( t
			(princ "Human wins Coin Toss!") (terpri)
			"Human"  )

	)
)
