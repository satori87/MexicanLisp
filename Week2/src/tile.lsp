;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     tile.lsp has a few functions relevant to tiles specifically
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getTileValue
; Purpose: returns the total pip count of a domino tile e.g. list of 2 integers
; Parameters: tile e.g. list of 2 integers
; Algorithm: add the first and second elements together
; Return Value: sum of elements of this list of 2 integers aka tile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getTileValue (tile)
	(+ (first tile) (getNth 2 tile) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: isDouble
; Purpose: returns whether or not this tile is a double
; Parameters: tile to examine
; Algorithm: if first and second element are equal, this tile is a double
; Return Value: t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun isDouble (tile)
	(cond
		( (null tile)
			() )
		( t
			(equal (first tile) (first (rest tile) ) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getPriorityTiles
; Purpose: Simply returns a list of all the single tiles, in order of descending 
;			total pip count
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getPriorityTiles ()
	;this helps to simplify AI logic. Simply iterate through this list and seeing if that tile 
	;can be played anywhere. All other factors being equal (e.g. markers, doubles)
	;this is ordered by best points possible
	'(  (8 9) (7 9) (6 9) (7 8) (5 9) (6 8) (4 9) (5 8) (6 7) (3 9)
		(4 8) (5 7) (2 9) (3 8) (4 7) (5 6) (1 9) (2 8) (3 7) (4 6)
		(0 9) (1 8) (2 7) (3 6) (4 5) (0 8) (1 7) (2 6) (3 5) (0 7)
		(1 6) (2 5) (3 4) (0 6) (1 5) (2 4) (0 5) (1 4) (2 3) (0 4)
		(1 3) (0 3) (1 2) (0 2) (0 1) )
)