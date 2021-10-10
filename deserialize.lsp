;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     deserialize.lsp holds functions related to breaking down the
;	game object into its constituents. it also handles loading game
;	from serialized file
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: loadGame
; Purpose: Obtains a valid filename from user and loads the game state from it
; Parameters: N/A
; Algorithm: Once valid path is obtained and stream opened, the contents are read
;			and processed slightly. For simplicity, computer train is fully mirrored,
;			and an 11th element must be added to game list representing whether or not 
;			computer and human players passed their turns
; Return Value: the game object obtained from a valid file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun loadGame ()
	(let ( (ins (openValidFile) ) )
		(cond 
			( (null ins)
				(loadGame) )
			( t
				; now to add 11th element and to reverse computerTrain, also reverse each tile!
				(let ( (game (append (read ins) (list (list () () ) ) ) ) )					
					(setComputerTrain game (reverseEach (reverseList (getComputerTrain game) ) ) )
				) )
		)
	)
)

; The game object is ordered as follows:
; roundNumber, computerScore, computerHand, computerTrain, humanScore, humanHand, humanTrain, mexicanTrain, boneyard, nextPlayer
; an 11th element not found in file, playerPassed information, is a list of booleans representing whether computer or human passed
; their last turn

; these function simply abstract away from these numeric cell numbers

(defun getRoundNumber (game)
	(getNth 1 game)
)

(defun getComputerScore (game)
	(getNth 2 game)
)

(defun getComputerHand (game)
	(getNth 3 game)
)

(defun getComputerTrain (game)
	(getNth 4 game)
)

(defun getHumanScore (game)
	(getNth 5 game)
)

(defun getHumanHand (game)
	(getNth 6 game)
)

(defun getHumanTrain (game)
	(getNth 7 game)
)

(defun getMexicanTrain (game)
	(getNth 8 game)
)

(defun getBoneyard (game)
	(getNth 9 game)
)

(defun getNextPlayer (game)
	(getNth 10 game)
)

(defun getComputerPassed (game)
	(getNth 1 (getNth 11 game) )
)

(defun getHumanPassed (game)
	(getNth 2 (getNth 11 game) )
)



