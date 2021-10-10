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



