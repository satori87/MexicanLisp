(defun startRound (game)
	(cond
		( (null (getBoneyard game) )
			(startRound (setBoneyard game (newBoneyard) ) ) )
		( (< (getListLength (getComputerHand game) ) 16)
			(startRound (setBoneyard (setComputerHand game (addTileToHand '"Computer" (getComputerHand game) (getBoneyard game) ) ) (rest (getBoneyard game) ) ) ) )
		( (< (getListLength (getHumanHand game) ) 16)
			(startRound (setBoneyard (setHumanHand game (addTileToHand '"Human" (getHumanHand game) (getBoneyard game) ) ) (rest (getBoneyard game) ) ) ) )
		( (null (getNextPlayer game) )
			(startRound  (setNextPlayer game (determineFirstPlayer (getComputerScore game) (getHumanScore game) ) ) ) )
		( t
			(format t '"Starting Round ~d. ~d goes first" (getRoundNumber game) (getNextPlayer game) ) (terpri)
			game )
	)
)

(defun playRound (game)
	(printRound game)
	(cond 
		( (null (isRoundOver game) )
			(cond
				( (equal (getNextPlayer game) "Computer")
					(playRound (takeComputerTurn game) ) )
				( (equal (getNextPlayer game) "Human")
					(playRound (takeHumanTurn game) ) )
			) )
		( t
			(endRound game) )
	)
)

(defun isRoundOver (game)
	t
	;either hand is empty or (boneyard is empty + both players passed)
)

(defun endRound (game)
	(format t "Round ~d over! Tallying scores" (getRoundNumber game) ) (terpri)
	game
)

(defun printRound (game)
	(format t "Round ~d | Computer Score: ~d | Human Score: ~d" (getRoundNumber game) (getComputerScore game) (getHumanScore game) ) (terpri)
	(printListLn '"Computer Hand  : " (getComputerHand game) ) 
	(printListLn '"Human Hand     : " (getHumanHand game) )
	(printListLn '"Computer Train : " (getComputerTrain game) )
	(printListLn '"Human Train    : " (getHumanTrain game) )
	(printListLn '"Mexican Train  : " (getMexicanTrain game) )
	(format t   '"Boneyard (~2,'0d)  :" (getListLength (getBoneyard game) ) )
	(printTop (getBoneyard game) )
	(format t '"It is ~d's turn" (getNextPlayer game) ) (terpri)
)

(defun getEngine (roundNumber)
	(let (
		( engineHalf (- 9 (- (mod roundNumber 10) 1)) ) )
		(list engineHalf engineHalf)
	)
)

(defun determineFirstPlayer (computerScore humanScore)
	(cond
		( (= computerScore humanScore )
			(coinToss) )
		( (< computerScore humanScore )
			'"Computer" )
		( (> computerScore humanScore )
			'"Human" )
	)
)

(defun coinToss ()
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
