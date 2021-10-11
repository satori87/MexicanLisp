;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     train.lsp has functions related to trains in the first section, 
;		and the second section is functions pertaining to playing tiles
;		to trains
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: remMarker
; Purpose: returns the train without any marker
; Parameters: train, a left to right train
; Algorithm: if train is null, return it
;			if the last element of the train is not a list, it must be a marker
;				so return all but that last element
;			otherwise just return train
; Return Value: train, guaranteed to not have a marker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remMarker (train)
	(cond
		( (null train)
			train )
		( (equal (listp (getLast train) ) () )
			(allButLast train) )
		( t
			train)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: hasMarker
; Purpose: returns whether or not this train has a marker
; Parameters: a left to right train to examine
; Algorithm: determines if train has marker by checking its last element.
;				if last element is a list, or this train is null, return nil
;				otherwise return t
; Return Value: t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hasMarker (train)
	(cond
		( (null train)
			() )
		( (equal (listp (getLast train) ) () )
			t )
		( t
			() )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: setMarker
; Purpose: sets or removes marker on train as appropriate
; Parameters: left-to-right oriented train, boolean marker
; Algorithm: if setting marker (t), make sure it doesnt have one first
;				if removing marker, go ahead and do it as remMarker has its own checks
;				otherwise just return the train
; Return Value: t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setMarker (train marker)
	(cond
		( (and marker (null (hasMarker train) ) )
			(append train (list 'M) ) )
		( (null marker)
			(remMarker train) )
		( t
			train )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: isAnyOrphans
; Purpose: returns whether or not there are any orphan doubles in the game
; Parameters: entire game object
; Algorithm: simply checks if the list of orphan trains has any trues
; Return Value: t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun isAnyOrphans (game)
	(listContains (getOrphanTrains game) t )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getOprhanTrains
; Purpose: creates a list of booleans representing whether or not each train has
;			an orphan double or not
; Parameters: entire gamne object
; Algorithm: To prevent false positives from the game's engine, we check that the list
;			of player trains are greater than 1 (0 for mexican train since it doesnt
;			contain engine)
;			the length check is ANDed with a double-check of the last element of the train
; Return Value: a list of booleans for each train if it has an orphan double or not
;				e.g. (T T nil) if can play on Computer Train, Human Train, but not Mexican
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getOrphanTrains (game)
	(list
		(and (isDouble (getLast (remMarker (getComputerTrain game) ) ) ) (> (getListLength (getComputerTrain game) ) 1 ) )
		(and (isDouble (getLast (remMarker (getHumanTrain game) ) ) ) (> (getListLength (getHumanTrain game) ) 1 ) )
		(and (isDouble (getLast (getMexicanTrain game) ) ) (> (getListLength (getMexicanTrain game) ) 0 ) )
	)	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getTrain
; Purpose: returns train for corresponding number 1 (computer), 2 (human), or 3 (mexican)
; Parameters: entire game object, desired train number
; Algorithm: simple conditional to interpret this integer, and use the corresponding
;				get function
; Return Value: the train for the given train number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getTrain (game trainNumber)
	(cond
		( (= trainNumber 1)
			(getComputerTrain game) )
		( (= trainNumber 2)
			(getHumanTrain game) )
		( (= trainNumber 3)
			(getMexicanTrain game) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getEndValue
; Purpose: Returns the number of pips at the end of the train. e.g., the side of the last
;			tile that faces away from the rest of the train
; Parameters: the engine, left to right oriented train
; Algorithm: get the last value of the last value, results in the end of the train.
;				if train is empty, use the engine
; Return Value: number of pips at end of train
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getEndValue (game train)
	(cond
		( (null train)
			(first (getEngine (getRoundNumber game) ) ) )
		( t
			(getLast (getLast (remMarker train) ) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getTrainName
; Purpose: Returns the name of the train for given train number
; Parameters: trainNumber 1 2 or 3
; Algorithm: 
; Return Value: Text or Error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getTrainName (trainNumber)
	(cond
		( (= trainNumber 1)
			'"Computer Train" )
		( (= trainNumber 2)
			'"Human Train" )
		( (= trainNumber 3)
			'"Mexican Train" )
		( t
			'"ErrorTrain: ~d" trainNumber )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;					TRAIN FUNCTIONS RELATED TO PLAYING TILES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: canPlayHandToTrain
; Purpose: Returns whether or not the specified hand contains at least one valid tile
;			playable against specified train. only checks legality of play,
;			 without considering rest of the table
; Parameters: game object, a left-to-right train, a player hand
; Algorithm: iterate recursively through hand, trying each tile with
;				canPlayTileToTrain
; Return Value: t if hand has at least 1 playable tile, otherwise nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun canPlayHandToTrain (game train hand)
	(cond
		( (null hand)
			;no
			() )
		( (canPlayTileToTrain game train (first hand) )
			;this tile can be played to the hand, return t
			t )
		( t
			;this tile doesnt work, keep trying
			(canPlayHandToTrain game train (rest hand) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: canPlayTileToTrain
; Purpose: Returns whether or not this tile can legally be played to this train
; Parameters: the game object, the specified train, the specified tile
; Algorithm: If train is null, return nil
;				if either side of the tile matches the end value of the train, return t
;				otherwise, nil
; Return Value: t or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun canPlayTileToTrain (game train tile)
	;if either of the values in tile matches the LAST value in train, return true
	(cond
		( (null train) 
			;mexican train is empty, so use knowledge of round inside game in order to
			;simulate a train with the engine on it
			(canPlayTileToTrain game (list (getEngine (getRoundNumber game) ) ) tile) )
		( (= (getEndValue game train ) (first tile) )
			t )
		( (= (getEndValue game train ) (getLast tile) )
			t )
		( t
			nil )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: finalizePlay
; Purpose: finalizing a play encompasses:
;				removing marker if player players to their own train
;				creating a list of the modified game object and the tile played
;				to return up the chain
; Parameters: the game object, train number, tile, and player number
; Algorithm: If train number equals player number, remove marker before forming list
; Return Value: list of (modfifiedGame tilePlayed)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun finalizePlay (game trainNumber train tile playerNumber)
	(cond
		( (= playerNumber 2)
			;since human help doesnt result in automatic play, we need to announce the play
			(format t "Human plays ~d to ~d" tile (getTrainName trainNumber) ) (terpri) )
	)
	(cond
		( (= trainNumber 1)
			(cond
				( (= playerNumber 1)
					;computer played to computer train, remove marker and return play
					(list (setComputerTrain game (remMarker train) ) tile) )
				(t
					;human played to human train, return play
					(list (setComputerTrain game train) tile) )
			) )			
		( (= trainNumber 2)
			(cond
				( (= playerNumber 2)
					;human played to human train, remove marker and return play
					(list (setHumanTrain game (remMarker train) ) tile) )
				(t
					;computer played to human train, return play
					(list (setHumanTrain game train) tile) )
			) )		
		( (= trainNumber 3)
			;play was to mexican train, return play
			(list (setMexicanTrain game train) tile) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: playTileToTrain
; Purpose: plays the tile to the train and then finalizes the play
; Parameters: game object, train number (1-3), tile, player number (1-3)
; Algorithm: 	first, make note of whether or not train has marker
;				check which way to add file (if it needs flipping), 
;				remove marker from train
;				add tile to train
;				reset marker status with set marker
;				pass info into finalizePlay to remove personal marker
;				and formulate our final return for the play
; Return Value: the finalized play e.g. list of (game tilePlayed)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun playTileToTrain (game trainNumber tile playerNumber)
	(let* ( (train (getTrain game trainNumber) )
			(marker (hasMarker train) )
		  )
		(cond
			( (equal (getEndValue game train) (first tile) )
				;no need to flip tile, add to train and put marker back if it was there
				(finalizePlay game trainNumber (setMarker (append (remMarker train) (list tile) ) marker) tile playerNumber) )
			( (equal (getEndValue game train) (getLast tile ) )
				;flip tile before adding to train, then put marker back if it was there
				(finalizePlay game trainNumber (setMarker (append (remMarker train) (list (reverseList tile) ) ) marker) (reverseList tile) playerNumber) )
			( t
				(format t "Fatal error in playTileToTrain. ~d ~d ~d ~d" game trainNumber tile playerNumber) (terpri)
				(quit) )
		)	
	)
)