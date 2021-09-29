

(defun mexicanTrain ()  
	(load "C:/lisp/boneyard.lsp")
	(load "C:/lisp/utility.lsp")
	(princ "Welcome to Mexican Train") (terpri)
	(introMenu)
	(princ "Thank you for playing. Goodbye.")
)

(defun introMenu()
	(displayIntroMenu)
	(processIntroMenuChoice (getValidNumber 1 3 "Make your Selection") ) 
)

(defun displayIntroMenu()
	(terpri)
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
	(startRound 1 0 () () 0 () () () () () )
)

(defun startRound(roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer)
	(cond
		( (null boneyard)
			(startRound roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain (newBoneyard) nextPlayer) )
		( (< (getListLength computerHand) 16)
			(startRound roundNumber computerScore (addTileToHand '"Computer" computerHand boneyard) computerTrain humanScore humanHand humanTrain mexicanTrain (rest boneyard) nextPlayer) )
		( (< (getListLength humanHand) 16)
			(startRound roundNumber computerScore computerHand computerTrain humanScore (addTileToHand '"Human" humanHand boneyard) humanTrain mexicanTrain (rest boneyard) nextPlayer) )
		( (null nextPlayer)
			(startRound roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard (coinToss) ) )
		( t
			(format t '"Starting Round ~d" roundNumber) (terpri)
			(playRound roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer) )
	)
)

(defun playRound (roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer)
	(printRound roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer)
)

(defun printRound (roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer)
	(format t "Round ~d | Computer Score: ~d | Human Score: ~d" roundNumber computerScore humanScore) (terpri)
	(printList '"Computer Hand  : " computerHand) 
	(printList '"Computer Train : " computerTrain) 
	(printList '"Human Hand     : " humanHand)
	(printList '"Human Train    : " humanTrain)
	(printList '"Mexican Train  : " mexicanTrain)
	(format t   '"Boneyard (~2,'0d)  :" (getListLength boneyard) )
	(printTop boneyard)
	(printTurn nextPlayer)
)

(defun printTurn(nextPlayer)
	(format t '"It is ~d's turn" nextPlayer) (terpri)
)


(defun printTop(boneyard)
	(printList '" " (list (first boneyard) ) )
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

(mexicanTrain)