;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     boneyard.lsp contains all functions related to boneyard
;

(defun rawBoneyard ()
	;Full 55-tile double-9 set
	;it is shuffed for normal play, but also has point priority pre-baked in
	;to simplify AI logic. Simply iterate through this list and seeing if that tile 
	;can be played anywhere
	'(  (9 9) (8 8) (7 7) (6 6) (5 5) (4 4) (3 3) (2 2) (1 1) (0 0)
		(8 9) (7 9) (6 9) (7 8) (5 9) (6 8) (4 9) (5 8) (6 7) (3 9)
		(4 8) (5 7) (2 9) (3 8) (4 7) (5 6) (1 9) (2 8) (3 7) (4 6)
		(0 9) (1 8) (2 7) (3 6) (4 5) (0 8) (1 7) (2 6) (3 5) (0 7)
		(1 6) (2 5) (3 4) (0 6) (1 5) (2 4) (0 5) (1 4) (2 3) (0 4)
		(1 3) (0 3) (1 2) (0 2) (0 1) )
)

;Simply return a shuffled boneyard
(defun newBoneyard()
	(getShuffledList (rawBoneyard) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: addTileToHand
; Purpose: Adds the top tile of boneyard to hand. Does NOT alter boneyard.
;			This function makes assumptions that boneyard is NOT empty.
;			The responsibility for checking is done elsewhere
; Parameters: player name, hand, boneyard
; Algorithm: the first element of boneyard is the top. Add it to hand and report.
; Return Value: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun addTileToHand(name hand boneyard)
	(format t "~d drew a ~d from the boneyard" name (first boneyard) ) (terpri)
	(append hand (list (first boneyard)) )
)

;Prints top of boneyard or EMPTY
(defun printTop(boneyard)
	(cond
		( (null boneyard)
			(princ 'EMPTY) )
		( t
			(format t "~d" (first boneyard) ) )
	)
	(terpri)
)