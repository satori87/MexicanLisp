;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     game.lsp contains all functions pertaining to Game flow, starting, ending, etc
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: initGame
; Purpose: Display intro menu and process choice
; Parameters: N/A
; Algorithm: N/A
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun initGame ()
	(displayIntroMenu)
	(processIntroMenuChoice (getValidNumber 1 3 "Make your Selection") ) 
)

;Prints the intro menu
(defun displayIntroMenu ()
	(terpri) (princ "Welcome to Mexican Train") (terpri)
	(princ "(1) Resume previous game") (terpri)
	(princ "(2) New game") (terpri)
	(princ "(3) Quit the game") (terpri)
)

;1 for resume game, 2 for new game, 3 to quit
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

;simply play a loaded game
(defun resumeGame ()
	(playGame (loadGame) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: newGame
; Purpose: Starts a new game
; Parameters: N/A
; Algorithm: Announce new game
;			 Initiate new game object with round number 1, 0 scores, empty hands/trains/etc
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun newGame ()
	(princ "Starting New Game") (terpri)
	(playGame (startRound (list 1 0 () () 0 () () () () () (list () () ) ) ) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: playGame
; Purpose: The main game loop. Handles scoring and declaring winner as well as advancing
;			to next round
; Parameters: 
; Algorithm: iterates with tail recursion, once per round. Calculates new scores from
;			completed round info
;			Then prompts human to keep playing
;			If not, declare winner
;			If yes, recursively iterate
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun playGame (game)
	(let(
		( scores (tallyScores (playRound game) ) ) )
		(cond
			( (equal (getValidNumber 1 2 "Do you wish to keep playing? (1) Yes (2) No") 2)
				(announceWinner scores) )
			( t
				(playGame (nextRound scores game) ) )
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: announceWinner
; Purpose: Analyze scores and announce winner
; Parameters: list of (computer score human score)
; Algorithm: N/A
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun announceWinner (scores)
	(cond
		( (< (first scores) (getLast scores) )
			;computer won
			(princ "Computer Wins!!") (terpri) )
		( (> (first scores) (getLast scores) )
			;human won
			(princ "Human Wins!!") (terpri) )
		( t
			;draw
			(princ "Game is a draw! How anticlimactic.") (terpri) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: nextRound
; Purpose: Advances to next round, carrying over prerequisite info from old round
; Parameters:  last round scores, last game object
; Algorithm: start a new round with new game object, using only scores and round number
;			from old game object as a reference
; Return Value: the new game object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nextRound (scores game)
	;return a new round with 1 higher round number, empty trains, empty hands, empty boneyard
	;set players passed to false here as well, when thats implemented
	;Round arguments are in same order as serialization file, plus 1 more field to hold who has passed	
	;roundNumber computerScore computerHand computerTrain humanScore humanHand humanTrain mexicanTrain boneyard nextPlayer passed
	(startRound (list (+ (getRoundNumber game) 1) (getNth 1 scores) () () (getNth 2 scores) () () () () () () ) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: tallyScores
; Purpose: Extracts hands and scores from old game object and uses to calculate new
;			scores
; Parameters: old game object
; Algorithm: Increment player scores via tallyHand, announce and return
; Return Value: list of (new total computerScore, new total humanScore)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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