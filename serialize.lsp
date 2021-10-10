;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     serialize.lsp handles all functions related to encoding game state
;	as well as function save entire game state to file
;

(defun setRoundNumber (game roundNumber)
	(setNth 1 game roundNumber)
)

(defun setComputerScore (game computerScore)
	(setNth 2 game computerScore)
)

(defun setComputerHand (game computerHand)
	(setNth 3 game computerHand)
)

(defun setComputerTrain (game computerTrain)
	(setNth 4 game computerTrain)
)

(defun setHumanScore (game humanScore)
	(setNth 5 game humanScore)
)

(defun setHumanHand(game humanHand)
	(setNth 6 game humanHand)
)

(defun setHumanTrain (game humanTrain)
	(setNth 7 game humanTrain)
)

(defun setMexicanTrain (game mexicanTrain)
	(setNth 8 game mexicanTrain)
)

(defun setBoneyard (game boneyard)
	(setNth 9 game boneyard)
)

(defun setNextPlayer (game nextPlayer)
	(setNth 10 game nextPlayer)
)

(defun setComputerPassed (game computerPassed)
	(setNth 11 game (list computerPassed (getNth 2 (getNth 11 game) ) ) )
)

(defun setHumanPassed (game humanPassed)
	(setNth 11 game (list (getNth 1 (getNth 11 game) ) humanPassed ) )
)

(defun saveGame (game)
	;gotta remove the engine from the mexican train
	;and remove the 11th element (playerPassed) from game
	(let ( (outs (openFileForSave) ) )
		(write (allButLast (setComputerTrain game (reverseEach (reverseList (getComputerTrain game) ) ) ) ) :stream outs )
		(close outs)
	)
	(quit)
)