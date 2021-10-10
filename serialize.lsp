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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: saveGame
; Purpose: process the game object for saving, obtain a pathname to save to, and save
; Parameters: the game object, still containing 11th element and mirrored computer train
; Algorithm: obtain valid pathname, open stream for write, mirror computer train to match
;			serialization file standard, and also trim the 11th element not found in files
; Return Value: QUITS after saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun saveGame (game)
	;gotta remove the engine from the mexican train
	;and remove the 11th element (playerPassed) from game
	(let ( (outs (openFileForSave) ) )
		(write (allButLast (setComputerTrain game (reverseEach (reverseList (getComputerTrain game) ) ) ) ) :stream outs )
		(close outs)
	)
	(quit)
)

; The game object is ordered as follows:
; roundNumber, computerScore, computerHand, computerTrain, humanScore, humanHand, humanTrain, mexicanTrain, boneyard, nextPlayer
; an 11th element not found in file, playerPassed information, is a list of booleans representing whether computer or human passed
; their last turn

; these function simply abstract away from these numeric cell numbers

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