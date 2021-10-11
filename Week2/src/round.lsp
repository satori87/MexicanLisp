;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     round.lsp contains all functions pertaining to Round
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: startRound
; Purpose: Sets up a new round. This entails starting with a freshly shuffled boneyard,
;			removing the engine, distributing 16 tiles to each player, putting the engine
;			on each player train, determining first player, and announcing the start of the round
; Parameters: partially formed game object in new of new round stuff
; Algorithm: Call startRound recursively, checking off the requirements one by one,
;				ultimately returning the game object with fresh round
; Return Value: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun startRound (game)
	(cond
		( (null (getBoneyard game) )
			(startRound (setBoneyard game (newBoneyard) ) ) )
		( (listContains (getBoneyard game) (getEngine (getRoundNumber game) ) )
			(startRound (setBoneyard game (remList (getBoneyard game) (getEngine (getRoundNumber game) ) ) ) ) )
		( (< (getListLength (getComputerHand game) ) 16)
			(startRound (setBoneyard (setComputerHand game (addTileToHand 'COMPUTER (getComputerHand game) (getBoneyard game) ) ) (rest (getBoneyard game) ) ) ) )
		( (< (getListLength (getHumanHand game) ) 16)
			(startRound (setBoneyard (setHumanHand game (addTileToHand 'HUMAN (getHumanHand game) (getBoneyard game) ) ) (rest (getBoneyard game) ) ) ) )
		( (null (getComputerTrain game) )
			(startRound (setComputerTrain game (list (getEngine (getRoundNumber game) ) ) ) ) )
		( (null (getHumanTrain game) )
			(startRound (setHumanTrain game (list (getEngine (getRoundNumber game) ) ) ) ) )
		( (null (getNextPlayer game) )
			(startRound  (setNextPlayer game (determineFirstPlayer (getComputerScore game) (getHumanScore game) ) ) ) )
		( t
			(format t '"Starting Round ~d. ~d goes first" (getRoundNumber game) (getNextPlayer game) ) (terpri)
			game
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: playRound
; Purpose: iterates recursively, first checking if round is over, then checking whose
; turn it is and executing it.			
; Parameters: game object
; Algorithm: First checks if round is over. If not, executes the players turn indicated
;			by getNextPlayer. 
; Return Value: In the end this returns the final game state to playGame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun playRound (game)
	(printRound game)
	(cond 
		( (isRoundOver game)
			(endRound game) )
		( (equal (getNextPlayer game ) 'Computer )
			(playRound (takeComputerTurn game) ) ) 
		( (equal (getNextPlayer game) 'Human )
			(playRound (takeHumanTurn game) ) )
		( t
			(format t "Fatal Error in playRound. GAMESTATE: ~d" game) (quit) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: isRoundOver
; Purpose: Determines if round is over
; Parameters: game object
; Algorithm: Check if computer hand is empty, player hand is empty.
;				If neither, check if boneyard is empty and both players passed
; Return Value: t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun isRoundOver (game)
	;either hand is empty or (boneyard is empty + both players passed)
	(cond
		( (null (getComputerHand game) )
			t )
		( (null (getHumanHand game) )
			t )
		( (and (null (getBoneyard game) ) (and (getComputerPassed game) (getHumanPassed game) ) )
			t )
		(t
			nil )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: endRound
; Purpose: Announces end of round
; Parameters: game object
; Algorithm:
; Return Value: the unmodified game object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun endRound (game)
	(format t "Round ~d over! Tallying scores" (getRoundNumber game) ) (terpri)
	game
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: printRound
; Purpose: print all info of the round state
; Parameters: game object
; Algorithm: Print round number, player scores
;			Print Computer, Human hands
;			Print Player Trains (have to mirror computer train as all trains are
;			stored left-to-right during gameplay.
;			Print Mexican Train
;			Print boneyard
;			Print whose turn it is
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun printRound (game)
	(format t "Round ~d | Computer Score: ~d | Human Score: ~d" (getRoundNumber game) (getComputerScore game) (getHumanScore game) ) (terpri)
	(printListLn '"Computer Hand  : " (getComputerHand game) ) 
	(printListLn '"Human Hand     : " (getHumanHand game) )
	(printList '"Player Trains  : " (reverseEach (reverseList (rest (getComputerTrain game) ) ) ) )
	(format t "<-*~d*->" (getEngine (getRoundNumber game) ) )
	(printListLn '"" (rest (getHumanTrain game) ) )
	(printListLn '"Mexican Train  : " (getMexicanTrain game) )
	(format t   '"Boneyard (~2,'0d)  : " (getListLength (getBoneyard game) ) )
	(printTop (getBoneyard game) )
	(format t '"It is ~d's turn" (getNextPlayer game) ) (terpri)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getEngine
; Purpose: Returns a tile representing the engine of the round
; Parameters: round number
; Algorithm: an engine is a double tile of n, where n is the 9 minus (modulus of the round number
;				minus 1)
; Return Value: tile representing engine of round
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getEngine (roundNumber)
	(let (
		( engineHalf (- 9 (- (mod roundNumber 10) 1)) ) )
		(list engineHalf engineHalf)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: determineFirstPlayer
; Purpose: Determines first player of a new round
; Parameters: computer score, human score
; Algorithm: If scores are equal (or both 0), do a coin toss
;				otherwise, lower score goes first
; Return Value: symbol representing player who goes first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun determineFirstPlayer (computerScore humanScore)
	(cond
		( (= computerScore humanScore )
			(coinToss) )
		( (< computerScore humanScore )
			'Computer )
		( (> computerScore humanScore )
			'Human )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: coinToss
; Purpose: Performs a coin toss to see who goes first
; Parameters: N/A
; Algorithm: Prompts player for 1 or 2 to "call the toss", but really just
;			Uses a 50/50 random number, player cant tell difference
; Return Value: symbol representing player who goes first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun coinToss ()
	(getValidNumber 1 2 "Human player call (1) Heads or (2) Tails")
	(cond
		( (= (random 2) 0)
			(princ "Computer wins Coin Toss!") (terpri)
			'Computer )
		( t
			(princ "Human wins Coin Toss!") (terpri)
			'Human  )

	)
)
