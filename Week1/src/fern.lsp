;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Name     : Eric Fernandes                                         
;  Project  : Domino Game / Lisp             
;  Class    : Organization of Programming Langauages - Amruth Kumar  
;  Date     : 10/27/99                                                
;  File     : dominoes.cl                  
;  Platform : Windows 98 with Allegro CL Lite 5.0.1           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  run 
;  PARAMETERS  :  
;  RETURNS     :  
;  DESCRIPTION :  Function to start execution of the game. Performs
;					   initializtion and starts the first game.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run ()
	
	(DisplayWelcomeScreen)
	(playDominoes 1 0 0 (GetOpeningPlayer) (InitializeEndpoints) (DrawHand 2 6 (DrawHand 1 6 ( shuffleDominoes 27 (shuffleDominoes 27 (CreateDominos))))) (InitializeBoard 75 25))
	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  demorun 
;  PARAMETERS  :  
;  RETURNS     :  
;  DESCRIPTION :  Function to start execution of the game using a sef-running
;                 demo mode (computer plays both players). Performs
;                 initializtion and starts the first game.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demorun ()
	
	(DisplayWelcomeScreen)
	(format t "~%~%~%SELF-RUNNING DEMO MODE~%~%~%")
	(playDominoes 0 0 0 (GetOpeningPlayer) (InitializeEndpoints) (DrawHand 2 6 (DrawHand 1 6 ( shuffleDominoes 27 (shuffleDominoes 27 (CreateDominos))))) (InitializeBoard 75 25))
	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  InitializeBoard 
;  PARAMETERS  :  width, height
;  RETURNS     :  Board list
;  DESCRIPTION :  creates and initialzies the nested lists for the Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun InitializeBoard (width height)

    (cond
	( (equal height 0) () )
	( t
	  (cons (MakeRow width) (InitializeBoard width (- height 1))) ) )
	  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  MakeRow
;  PARAMETERS  :  width
;  RETURNS     :  list of characters for one row
;  DESCRIPTION :  Called by InitializeBoard to contruct a row list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                     )
	

(defun MakeRow (width)
	
    (cond
	( (equal width 0) () )
	( t
	  (cons '~ (MakeRow (- width 1))) 
					) )  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  OutputEndpoints
;  PARAMETERS  :  endpoints
;  RETURNS     :  (output)
;  DESCRIPTION :  Outputs information on the available positions (wrapper)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun OutputEndpoints (endpoints)

	(format t "~%~%Available positions~%")
	(format t "--------------------~%")

	(OutputEndpoints_op endpoints)  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  OutputEndpoints_op
;  PARAMETERS  :  endpoints
;  RETURNS     :  (output)
;  DESCRIPTION :  Recusrsive function called by OutputEndpoints to display
;                 the endpoint data.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun OutputEndpoints_op (endpoints)

	(cond
	( (null endpoints)
		() )
	( (equal (car (car endpoints)) 99)
	  	(OutputEndpoints_op (cdr endpoints)))
	( t 
	  (princ (car (car endpoints)))
	  (princ " at (")
	  (princ (car (car (cdr (car endpoints)))))
	  (princ ",")
	  (princ (car (cdr (car (cdr (car endpoints)))))) 
	  (princ ")")
	  (princ #\NEWLINE )
	  (OutputEndpoints_op (cdr endpoints)) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  OutputBoard
;  PARAMETERS  :  board
;  RETURNS     :  (output)
;  DESCRIPTION :  Displays the board (wrapper)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun OutputBoard (board)

	(format t "~%~%Contents of board~%")
	(format t "--------------------")

	(OutputBoard_op board)  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  OutputBoard_op
;  PARAMETERS  :  board
;  RETURNS     :  (output)
;  DESCRIPTION :  Recursive function called by OutputBoard to dosplay the board.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun OutputBoard_op (board)

	(cond

	 ( (equal (car board) () ) 
	   () )
	 ( t 
	   (princ #\NEWLINE )
	   (OutputBoardLine_op (car board))
	   (OutputBoard_op (cdr board)) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  OutputBoardLine_op
;  PARAMETERS  :  boardline
;  RETURNS     :  (output)
;  DESCRIPTION :  Recersive function to display one line from the board.
;                 Called by OutputBoard_op.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun OutputBoardLine_op (boardLine)

	(cond

	( (equal (car boardLine) () )
	  () )
	( t 
	  (princ (car boardLine))
	  (OutputBoardLine_op (cdr boardLine)) )
					)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  DrawTile
;  PARAMETERS  :  board, a, b, x, y, direction
;  RETURNS     :  modified Board
;  DESCRIPTION :  Draws the supplied tile on the Board using the supplied
;                 values, coordinates and direction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun DrawTile (board a b x y direction)

; 0 = right
; 1 = left
; 2 = down
; 3 = up

	(cond
	( (equal direction 0)
		(PlaceValue (PlaceValue (PlaceValue (PlaceValue board '- x y) a ( + x 1) y) b (+ x 2) y) '- (+ x 3) y) )
	( (equal direction 1)
		(PlaceValue (PlaceValue (PlaceValue (PlaceValue board '- x y) a ( - x 1) y) b (- x 2) y) '- (- x 3) y) )
	( (equal direction 2)
		(PlaceValue (PlaceValue (PlaceValue (PlaceValue board '- x y) a x ( + y 1) ) b x (+ y 2)) '- x (+ y 3)) )
	( (equal direction 3)
		(PlaceValue (PlaceValue (PlaceValue (PlaceValue board '- x y) a x ( - y 1) ) b x (- y 2)) '- x (- y 3)) )
	(t () )
			) 
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  PlaceValue
;  PARAMETERS  :  board, a, x, y
;  RETURNS     :  (modified board)
;  DESCRIPTION :  Recursive function called by DrawTile to place the target
;                 tile on the Board. Called once for each character in
;                 the tile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun PlaceValue (board a x y )

	(cond

	 ( (equal (car board) () )
		 () )
	 ( (equal y 0)
		 (cons (PlaceValue_ProcessLine (car board) a x) (cdr board)) )
	 ( t 
	    (cons (car board) (PlaceValue (cdr board) a x (- y 1) )) ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  PLaceValue_ProcessLine
;  PARAMETERS  :  boardline, a, x
;  RETURNS     : (modified line from board)
;  DESCRIPTION :  Recurses through one line from the board to set the
;                 target value for drawing the tile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PlaceValue_ProcessLine (boardLine a x )

	(cond

	( (equal (car boardLine) () )
		() )
	( (equal x 0)
		(cons a (cdr boardLine)) )
	( t 
		(cons (car boardLine) (PlaceValue_ProcessLine (cdr boardLine) a (- x 1) ) ) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  DisplayWelcomeScreen
;  PARAMETERS  :  
;  RETURNS     :  (output)
;  DESCRIPTION :  Displays welcome screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun DisplayWelcomeScreen ()
	(format t "~%~%Contents of board~%")
	(format t "~%~%~%~%~%Welcome To Dominos~%")
	(format t'"------------------~%~%")
	(format t "by Eric Fernandes~%")
	(format t "for Organization of Programming Languages~%")
	(format t "Amruth Kumar~%")
	(format t "Ramapo College~%")
	(format t "October 1999~%~%~%~%~%")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  dominoesOut 
;  PARAMETERS  :  dominoList, player
;  RETURNS     :  (output)
;  DESCRIPTION :  Outputs eitehr the boneyard or a player's hand as indictated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun dominoesOut (dominoList player)

	(cond
		((equal player 0)
			(format t "~%~%Contents of boneyard~%")
			(format t "--------------------~%") )
		((equal player 1)
			(format t "~%~%Your Hand~%")
			(format t "---------~%") )
		((equal player 2)
			(format t "~%~%Computer's Hand~%")
			(format t "---------------~%") )
		( t
			(format t "~%~%Undefined player number~%")
			(format t " -----------------------~%") 
		) 
	)
	;call recursive function to perform output
	(dominoesOut_op dominoList player)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  dominoesOut_op
;  PARAMETERS  :  dominoList, player
;  RETURNS     :  (output)
;  DESCRIPTION :  Performs the recursive outputting of a hand from the
;                 domino list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun dominoesOut_op (dominoList player)

	(cond 
		( (null dominoList)
			() )
		( t
			(cond 
				;output only if matches target owner
				((equal player (car (cdr (cdr (car dominoList)))))
					(princ (car (car dominoList)))
					(princ "|")
					(princ (car (cdr (car dominoList))))
					(princ " "))
				( t ()) 
			)
			;repeat with remaining dominoes
			(dominoesOut_op (cdr dominoList) player))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  shuffleDominoes
;  PARAMETERS  :  numDominoes, dominoList
;  RETURNS     :  modifed dominoList
;  DESCRIPTION :  Recursively shuffles the supplied dominoList.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun shuffleDominoes (numDominoes dominoList)

	(cond 

		( (equal numDominoes 0)
			() )
		( t
			(let (
				(value (random numDominoes)) )
			(cons (shuffle_GetValue dominoList value) (shuffleDominoes (- numDominoes 1) (shuffle_RemoveValue dominoList value))) 
		) ) 
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  shuffle_GetValue
;  PARAMETERS  :  dominoList, index
;  RETURNS     :  value indicated by index
;  DESCRIPTION :  Supplies the value indicated by teh randomly generated
;                 index for shuffling.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shuffle_GetValue (dominoList index)

	(cond 
		( (eql index 0 ) 
			(car dominoList) ) 
		( t
			(shuffle_GetValue (cdr dominoList) (- index 1)))  
	) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  shuffle_RemoveValue
;  PARAMETERS  :  dominoList, index
;  RETURNS     :  modified dominoList
;  DESCRIPTION :  Removed the value indicated by teh randomly generated
;                 index for shuffling.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shuffle_RemoveValue (dominoList index)

	(cond 
		( (eql index 0 ) 
			(cdr dominoList) ) 
		( t 
			(cons (car dominoList) (shuffle_RemoveValue (cdr dominoList) (- index 1)))
		) 
	) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  CreateDominos
;  PARAMETERS  :  
;  RETURNS     :  dominoList
;  DESCRIPTION :  Initializes the domino list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

)
(defun CreateDominos ()

	(list
		'(0 0 0) '(0 1 0) '(0 2 0) '(0 3 0) '(0 4 0) '(0 5 0) '(0 6 0)
					'(1 1 0) '(1 2 0) '(1 3 0) '(1 4 0) '(1 5 0) '(1 6 0)
								'(2 2 0) '(2 3 0) '(2 4 0) '(2 5 0) '(2 6 0)
											'(3 3 0) '(3 4 0) '(3 5 0) '(3 6 0)
														'(4 4 0) '(4 5 0) '(4 6 0)
					 												'(5 5 0) '(5 6 0)
																				'(6 6 0)) 
																							
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  InitializeEndpoints 
;  PARAMETERS  :  
;  RETURNS     :  Initialized endpoint list
;  DESCRIPTION :  Initializes the endpoint list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun InitializeEndpoints()
	
	(list
		(list 99 (list 0 0))
		(list 99 (list 0 0))
		(list 99 (list 0 0))
		(list 99 (list 0 0)) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  GetOpeningPlayer
;  PARAMETERS  :  
;  RETURNS     :  1 for player, 2 for computer
;  DESCRIPTION :  Selects the opening player by simulatign drawign tiles.
;                 The winner is announced and selected player returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun GetOpeningPlayer ()

	(format t "~%Drawing to determine opening player....~%")
	
	;simulate drawing tiles
	(let(
		(playerDrawA (random 6))
		(playerDrawB (random 6))
		(computerDrawA (random 6))
		(computerDrawB (random 6))  
		)

	(princ #\NEWLINE )
	(format t "~%You have          : ")
	(princ playerDrawA )
	(format t  " | " )
	(princ playerDrawB)
	(format t "~%The computer has  : ")
	(princ computerDrawA )
	(format t " | " )
	(princ computerDrawB)
	(princ #\NEWLINE )	
	
	;determine winner
	(cond 
		( (> (+ playerDrawA playerDrawB) (+ computerDrawA computerDrawB))
			(format t "~%You will go first.~%")
			(Pause)
			1 )
		( (< (+ playerDrawA playerDrawB) (+ computerDrawA computerDrawB))
			(format t "~%The computer will go first.~%")
			(Pause)
			2 )
		( t
			(format t "You will go first.~%")
			(Pause)
			1 ) ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  DrawNextCard 
;  PARAMETERS  :  player, dominoList
;  RETURNS     :  modified dominoList
;  DESCRIPTION :  Marks the next available card for the supplied player
;                 and returns the modified domino list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun DrawNextCard (player dominoList)

	(cond
		((null dominoList)
			(list 0 0 dominoList 0 0) )
		((equal (car (cdr (cdr (car dominoList)))) 0)
			(cons (list (car (car dominoList)) (car (cdr (car dominoList))) player) (cdr dominoList)) )
		(t
			(cons (car dominoList) (DrawNextCard player (cdr dominoList)))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  DrawHand
;  PARAMETERS  :  player, numcards, dominoList
;  RETURNS     :  modified dominoList
;  DESCRIPTION :  Selects the supplied number of cards for the supplied
;                 player by marking the tiles and owned by the player.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun DrawHand (player numCards dominoList)

	(cond
		( (equal numCards 0)
			dominoList )
		( t
			(DrawHand player (- numCards 1) (DrawNextCard player dominoList)) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  AnyDominoesInBoneyard 
;  PARAMETERS  :  dominoes, numDominoes
;  RETURNS     :  1 on yes / 0 on no
;  DESCRIPTION :  Determines if there are any tiles left in the boneyard.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun AnyDominoesInBoneyard (dominoes numDominoes)
	
	(cond
		( (equal numDominoes 0)
			0 )
		( (equal (car (cdr (cdr (car dominoes)))) 0)
			; a tile ends in 0 (owned by boneyard), so return yes
			1 )
		( t
			(AnyDominoesInBoneyard (cdr dominoes) (- numDominoes 1))))

			)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  ExistPlayableTile 
;  PARAMETERS  :  dominoes, numDominoes, player, endpoints
;  RETURNS     :  list of endpoint and tile number or -1 for failure
;  DESCRIPTION :  Determines if there are any playable tiles in the player's
;                 hand and eitehr returns list of endpoint and tile number
;                 or -1 on failure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ExistPlayableTile (dominoes numDominoes player endpoints)

	(cond
		( (equal numDominoes 0)
			(list -1 0) ) ;none found 
		;check third endpoint
		( (> (ExistPlayableTile_Endpoint (car dominoes) player (car (cdr (cdr endpoints))) numDominoes) -1)
			(list 2 (ExistPlayableTile_Endpoint (car dominoes) player (car (cdr (cdr endpoints))) numDominoes)) )
		;check final endpoint
		( (> (ExistPlayableTile_Endpoint (car dominoes) player (car (cdr (cdr (cdr endpoints)))) numDominoes) -1)
			(list 3 (ExistPlayableTile_Endpoint (car dominoes) player (car (cdr (cdr (cdr endpoints)))) numDominoes)) )
		;check first endpoint
		( (> (ExistPlayableTile_Endpoint (car dominoes) player (car endpoints) numDominoes) -1)
			(list 0 (ExistPlayableTile_Endpoint (car dominoes) player (car endpoints) numDominoes)) )
		;check second enpoint
		( (> (ExistPlayableTile_Endpoint (car dominoes) player (car (cdr endpoints)) numDominoes) -1)
			(list 1 (ExistPlayableTile_Endpoint (car dominoes) player (car (cdr endpoints)) numDominoes)) )
		;repeat with next domino
		( t
     			(ExistPlayableTile (cdr dominoes) (- numDominoes 1) player endpoints) ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  ExistPlayableTile_Endpoint 
;  PARAMETERS  :  domino, player, endpoint, numDominoes
;  RETURNS     :  tile number or -1 for failure
;  DESCRIPTION :  Checks the supplied endpoint to see if the tile is playable
;                 at that position.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ExistPlayableTile_Endpoint (domino player endpoint numDominoes)
	

	(cond
		;see if the tile can be placed at this end
		( (and (equal (car(cdr(cdr domino))) player)
			(or
				(equal (car(cdr domino)) (car endpoint))
				(equal (car domino) (car endpoint))
			)
		  )		
			(- 27 numDominoes ) )
		
		;tile is not playable here
		( t
			-1
		)
			))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  playDominoes
;  PARAMETERS  :  mode playerScore, computerScore, player, endpoints, dominoList, board
;  RETURNS     :  
;  DESCRIPTION :  Main playing. loop. Recurses until a player wins. When the mode
;                 is 1, it is computer vs. player. Mode 0 indictaes demo mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun playDominoes (mode playerScore computerScore player endpoints dominoList board)

	(let*(
		;place the first tile on the board
		(combined (PlaceFirstTile player endpoints dominoList board))
		(newEndpoints (car combined))
		(newdominoList (car (cdr combined)))
		(newBoard (car (cdr (cdr combined)))) 
		
		;play the game
		(result (playDominoes_op mode (SwitchPlayer player) newEndpoints newdominoList newBoard))
		(newPlayerScore
		
		;determine new score
		(cond
			( (equal result 1)
				(+ playerScore 10) )	
			( (equal result 0)
				(+ playerScore 5) )
			( t
				playerScore )))	
		(newComputerScore
		(cond
			( (equal result 2)
				(+ computerScore 10) )	
			( (equal result 0)
				(+ computerScore 5) )
			( t
				computerScore )))	
	)
	
	(format t "CURRENT SCORES~%")
	(format t "--------------~%")
	(format t  "Player    : ")
	(print newPlayerScore)
	(format t "Computer : ")
	(print newComputerScore)

	;determine winner ro recursively play if no winner
	(cond
		( (>= playerScore 100 )
			(format t "~%YOU WIN!!!!!!!!! GAME OVER!~%") )
		( (>= computerScore 100 )
			(format t "~%COMPUTER WINS.... GAME OVER...~%") )
		( t
			(playDominoes mode newPlayerScore newComputerScore (GetOpeningPlayer) (InitializeEndpoints) (DrawHand 2 6 (DrawHand 1 6 ( shuffleDominoes 27 (shuffleDominoes 27 (CreateDominos))))) (InitializeBoard 70 20)))
	)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  PlaceFirstTile
;  PARAMETERS  :  player, endpoints, dominoList, board
;  RETURNS     :  modified endpoints, board and dominoList
;  DESCRIPTION :  Places the first tile on the board based on the
;                 player and returns a list of the modified endpoints.
;                 dominoList and board.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PlaceFirstTile (player endpoints dominoList board)

	(let* 
	(	(newdominoList (PlaceFirstTile_op1 player dominoList))
		(values (PlaceFirstTile_op2 newdominoList))
		(a (car values))
		(b (car (cdr values)))
		
		;draw tile to right
		(newBoard (DrawTile board b a 35 10 0))
		(newEndpoints (cons (list b (list 35 10)) (cdr (cdr endpoints))))
		(newEndpoints (cons (list a (list 38 10)) newEndpoints))
	)	
		(cond
			( (equal player 1)
				(format t "~%~%You have placed tile "))
			(t
				(format t "~%~%The computer has placed tile "))
		)
		(princ a)
		(princ "|")
		(princ b)
		(format t " on the board.~%~%")
		(Pause)
		(list newEndpoints newdominoList newBoard)
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  PlaceFirstTile_op1
;  PARAMETERS  :  player, dominoList
;  RETURNS     :  modifed domino list
;  DESCRIPTION :  First recursive function called by PlaceFirstTile wrapper.
;                 Sets the target tile as used in the list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PlaceFirstTile_op1 (player dominoList)

	(cond
		( (equal (car (cdr (cdr (car dominoList)))) player)
			(cons (list (car (car dominoList)) (car (cdr (car dominoList))) 3) (cdr dominoList)) )
		(t
			(cons (car dominoList) (PlaceFirstTile_op1 player (cdr dominoList))) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  PlaceFirstTile_op2
;  PARAMETERS  :  dominoList
;  RETURNS     :  values of first tile
;  DESCRIPTION :  First recursive function called by PlaceFirstTile wrapper.
;                 Returns the values of the first tile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PlaceFirstTile_op2 (dominoList)

	(cond
		( (equal (car (cdr (cdr (car dominoList)))) 3)
			(list (car (car dominoList)) (car (cdr (car dominoList)))) )
		(t
			(PlaceFirstTile_op2 (cdr dominoList)) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  ProcessSpinner 
;  PARAMETERS  :  a,b, x, y, endpoints
;  RETURNS     :  modifed endpoints list
;  DESCRIPTION :  If a sipinner ahs been encountered, validates the 3rd
;                 and 4th endpoints (for up and down direction).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ProcessSpinner (a b x y endpoints)

	(cond
		( (and
			(equal a b)
			(equal (car (car (cdr (cdr endpoints )))) 99)
		)
				(format t "~%~%SPINNER!!!!!!~%~%")
				;validate 3rd and 4th endpoints
				(list
					(car endpoints)
					(car (cdr endpoints))
					(list a (list x (+ y 1)))
					(list b (list x (- y 1)))
				)
			)
		;no spinner, so return unchanged endpoints
		( t
			endpoints))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  MarkFromPlayerTileNumber 
;  PARAMETERS  :  player, dominoList, index
;  RETURNS     :  modified domino list
;  DESCRIPTION :  Marks the supplied tile in teh list as havign been
;                 placed on the board.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun MarkFromPlayerTileNumber (player dominoList index)

	(cond
		( (null dominoList)
			() )
		( (equal index 0)
			(cons 
			(list (car (car dominoList)) (car (cdr (car dominoList))) 3 ) 
			(cdr dominoList)) )
		(t
			(cons (car dominoList) (MarkFromPlayerTileNumber player (cdr dominoList) (- index 1)))))
				)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  GetABFromPlayerTileNumber 
;  PARAMETERS  :  player, dominoList, index
;  RETURNS     :  list of values of target tile
;  DESCRIPTION :  Returns the values of the tile indicated by the index.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GetABFromPlayerTileNumber (player dominoList index)

	(cond
		;not found - return -1 for error
		( (null dominoList)
			(list -1 -1) )
		;target tile found
		( (equal index 0)
			(list (car (car dominoList)) (car (cdr (car dominoList))) ) )
		( t
			(GetABFromPlayerTileNumber player (cdr dominoList) (- index 1)) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  GetXYFromEndpointIndex
;  PARAMETERS  :  endpoints, targetEndpoint
;  RETURNS     :  x and y of specified endpoint 
;  DESCRIPTION :  Returns teh x and y corrdinates of the specified endpoint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GetXYFromEndpointIndex (endpoints targetEndpoint)

	(cond
		( (equal targetEndpoint 0)
			(list (car (car (cdr (car endpoints)))) (car (cdr (car (cdr (car endpoints)))))))
		( t
			(GetXYFromEndpointIndex (cdr endpoints) (- targetEndpoint 1 )) ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  AdjustEndpoint
;  PARAMETERS  :  endpoints, targetEndpoint, a, b, x, y
;  RETURNS     :  modified endpoints
;  DESCRIPTION :  Modifies teh endpoints for the newly played tile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun AdjustEndpoint (endpoints targetEndpoint a b x y)

		;based on teh endpoint used, add in the appropriate entry
		; for the newly played tile
		(cond
			( (equal targetEndpoint 0)
				(list
					(list b (list (+ x 3) y))
					(car (cdr endpoints))
					(car (cdr (cdr endpoints)))
					(car (cdr (cdr (cdr endpoints))))
				)
			)
			( (equal targetEndpoint 1)
				(list
					(car endpoints)
					(list b (list (- x 3) y))
					(car (cdr (cdr endpoints)))
					(car (cdr (cdr (cdr endpoints))))
				)
			)
			( (equal targetEndpoint 2)
				(list
					(car endpoints)
					(car (cdr endpoints))
					(list b (list x (+ y 3)))
					(car (cdr (cdr (cdr endpoints))))
				)
			)
			( (equal targetEndpoint 3)
				(list
					(car endpoints)
					(car (cdr endpoints))
					(car (cdr (cdr endpoints)))
					(list b (list x (- y 3)))
				)
			)
		)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  SwapValues 
;  PARAMETERS  :  dominoList, targetTile
;  RETURNS     :  modifed domino list
;  DESCRIPTION :  Swaps the a and b values of the target tile in the domino list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SwapValues (dominoList targetTile)

	(cond
		( (equal targetTile 0)
			(cons (list (car (cdr (car dominoList))) (car (car dominoList)) (car (cdr (cdr (car dominoList)))))
			(cdr dominoList)))
		( t
			(cons (car dominoList) (SwapValues (cdr dominoList) (- targetTile 1))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  GetEndpointValue
;  PARAMETERS  :  endpoints, targetEndpoint
;  RETURNS     :  tile value of supplied endpoint
;  DESCRIPTION :  Returns the tile value of the supplied endpoint.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GetEndpointValue (endpoints targetEndpoint)

	(cond 
		( (equal targetEndpoint 0)
			(car (car endpoints)) )
		( t
			(GetEndpointValue (cdr endpoints) (- targetEndpoint 1)))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  SwitchIfNecessary 
;  PARAMETERS  :  dominoList, targetEndpoint, endpoints, targetTile, abTemp
;  RETURNS     :  modified domino list
;  DESCRIPTION :  If necessary, switch teh tile values to rotate the tile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun SwitchIfNecessary (dominoList targetEndpoint endpoints targetTile abTemp)

 	(cond
		( (equal (GetEndpointValue endpoints targetEndpoint) (car abTemp))
			(list (car abTemp) (car (cdr abTemp)) dominoList) )
		( t
			(list (car (cdr abTemp)) (car abTemp) (SwapValues dominoList targetTile))))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  UserSelectedTile 
;  PARAMETERS  :  dominoList, endpoints
;  RETURNS     :  endpoint and tile number
;  DESCRIPTION :  Allows the user to select a tile to play. After a
;                 valid tile is selected, its tile number and the endpoint
;                 to be used are returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UserSelectedTile (dominoList endpoints)

	(let*(
		;accept values from user
		(tileNumber (GetdominoListIndexFromValues dominoList (AcceptTileValuesFromUser) 0))
		(selectedValues (CheckDominoForPlayability dominoList 1 endpoints tileNumber tileNumber))
		)
	(cond
		;check return value to determine if playable
		( (> (car selectedValues) -1)
			(format t "~%Tile will be played~%")
			selectedValues
		)
		( t
			;tile is not good - recurse
			(format t "~%That tile is not playable!~%")
			(UserSelectedTile dominoList endpoints)))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  AcceptTileValuesFromUser 
;  PARAMETERS  :  
;  RETURNS     :  list of values for the tile.
;  DESCRIPTION :  Accepts the 2 values of a tile from the user, validates
;                 their validity as a tile, and returns them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun AcceptTileValuesFromUser ()

	(let*(
		( a (GetNumericInput 0 6 "Enter the first value of the tile  : "))
		( b (GetNumericInput 0 6 "Enter the second value of the tile : "))
	)
		(cond
			;ensure both ater within range
			( (or (> a 6) (> b 6))
				(format t "~%Invalid tile value~%")
				(AcceptTileValuesFromUser) 
			)
			( t
				(list a b)
			)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  CheckDominoForPlayability 
;  PARAMETERS  :  dominoList, player, endpoints, tileNumber
;  RETURNS     :  endpoint and tile numebr or -1 on error
;  DESCRIPTION :  Checks the user selection to ensure that it is playable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CheckDominoForPlayability (dominoList player endpoints tileNumber index)

	(cond
	( (equal index -1)		
			(list -1 -1))
	
	( (equal index 0)
		;check all endpoints to see if playable there
				;check first endpoint
		(cond
		( (> (ExistPlayableTile_Endpoint (car dominoList) player (car endpoints) 0) -1)
			(list tileNumber 0) )
		;check second enpoint
		( (> (ExistPlayableTile_Endpoint (car dominoList) player (car (cdr endpoints)) 0) -1)
			(list tileNumber 1) )
		;check third endpoint
		( (> (ExistPlayableTile_Endpoint (car dominoList) player (car (cdr (cdr endpoints))) 0) -1)
			(list tileNumber 2) )
		;check final endpoint
		( (> (ExistPlayableTile_Endpoint (car dominoList) player (car (cdr (cdr (cdr endpoints)))) 0) -1)
			(list tileNumber 3) )
		
		;not playable on any endpoint
		( t
			(list -1 -1)))
		)
		(t
			(CheckDominoForPlayability (cdr dominoList) player endpoints tileNumber (- index 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  GetdominoListIndexFromValues 
;  PARAMETERS  :  dominoList, a, b
;  RETURNS     :  tile index
;  DESCRIPTION :  From teh user supplied tile, returns index into domino list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GetdominoListIndexFromValues (dominoList abCombined index)

	(cond
		( (null dominoList)
			-1 )
		;check for match in either direction
		( (or
			(and
				(equal (car (car dominoList)) (car abCombined))
				(equal (car (cdr (car dominoList))) (car (cdr abCombined)))
			)
			(and
				(equal (car (car dominoList)) (car abCombined))
				(equal (car (cdr (car dominoList))) (car (cdr abCombined)))
			)
		)
			;found, return index
			index )
		;check next tile
		( t
			(GetdominoListIndexFromValues (cdr dominoList) abCombined (+ index 1)) 
		)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  PlayTile 
;  PARAMETERS  :  mode, player, endpoints, dominoList, board, targetEndpoint, targetTile
;  RETURNS     :  modified lists
;  DESCRIPTION :  Places a tile on the board, updates its status in the domino
;                 list, and updates teh endpoints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun PlayTile (mode player endpoints dominoList board targetEndpoint targetTile)

	(let*(
		;automatic seelction for computer or request from user
		(selectedTileAndEndpoint
			(cond
				( (and
					(equal player 1)
					(equal mode 1)
				   )
					; at least 1 tile is playable and interactive mode
					(UserSelectedTile dominoList endpoints) )
				( t
					(list targetTile targetEndpoint ))))
		(selectedTile (car selectedTileAndEndpoint))
		(selectedEndpoint (car (cdr selectedTileAndEndpoint)))
		(abTemp (GetABFromPlayerTileNumber player dominoList selectedTile))
		(abDominoCombined (SwitchIfNecessary dominoList selectedEndpoint endpoints selectedTile abTemp))
		(a (car abDominoCombined))
		(b (car (cdr abDominoCombined)))
		
		;mark in domino list
		(newdominoList (MarkFromPlayerTileNumber player (car (cdr (cdr abDominoCombined))) selectedTile))
		(xyCombined (GetXYFromEndpointIndex endpoints selectedEndpoint))
		(x (car xyCombined))
		(y (car (cdr xyCombined))) 
		
		;draw on board
		(newBoard (DrawTile board a b x y selectedEndpoint ))
		
		;update endpoints
		(newEndpoints (ProcessSpinner a b x y (AdjustEndpoint endpoints selectedEndpoint a b x y))))

		;display success status
		(cond
			( (equal player 1)
				(format t "~%You have added tile ") )
			( t
				(format t "~%The computer has added tile ")))
		(princ a)
		(princ "|")
		(princ b)
		(Pause)
	
		;return modified lists
		(list newEndpoints newdominoList newBoard)
	
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  DrawFromBoneyard 
;  PARAMETERS  :  numDominoes, player, endpoints, dominoList
;  RETURNS     :  modified domino list
;  DESCRIPTION :  Selects the next availabel tile and reserves it for
;                 the player.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun DrawFromBoneyard (numDominoes player endpoints dominoList)


	(cond
		( (equal numDominoes 0)
			() )
		; tile is free if in boneyard (player 0)
		( (equal (car (cdr (cdr (car dominoList)))) 0)
			(cons (list (car (car dominoList)) (car (cdr (car dominoList))) player)
			(cdr dominoList)) )
		( t
			(cons (car dominoList) (DrawFromBoneyard (- numDominoes 1) player endpoints (cdr dominoList))) 
		) 
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  PlayerDomino 
;  PARAMETERS  :  dominoList, player
;  RETURNS     :  1 on yes / 0 on no
;  DESCRIPTION :  Determines whether teeh indicated player has domioned.
;                 Recurses through domino list until either a tile found
;                owned by teh player or teh end reached.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PlayerDomino (dominoList player)

	(cond
		( (null dominoList)
			1 )
		( (equal (car (cdr (cdr (car dominoList)))) player)
			0 )
		( t
			(PlayerDomino (cdr dominoList) player)))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  playDominoes_op 
;  PARAMETERS  :  mode, player, endpoints, dominoList, board
;  RETURNS     :  winner of game
;  DESCRIPTION :  Performs the handling of one game. The mode determines whether
;                 the player plays ro teh computer plays bith hands  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun playDominoes_op (mode player endpoints dominoList board)
	
	
	;collect garbage
	(gc)

	(cond
		
		;check for domino of either player
		( (equal (PlayerDomino dominoList 1) 1)
			(format t "~%DOMINO!!!!!    YOU WIN!!!!!!!!!!!!!~%~%") 
			1
		)
		( (equal (PlayerDomino dominoList 2) 1)
			(format t "~%DOMINO!!!!!    COMPUTER WINS!!!!!!!!!!!!!~%~%") 
			2
		)
		;determine if any tiles can be played
		( (equal (car (ExistPlayableTile dominoList 27 player endpoints)) -1)
			(cond
				;no playable tiles
				;see if any left in boneyard
				( (equal (AnyDominoesInBoneyard dominoList 27) 1) 
					(cond
						( (equal player 1)
							(format t "~%You have to draw from the boneyard......~%~%"))
						( t 
							(format t "~%The computer is drawing from the boneyard......~%~%")))
					;draw tile and recurse
					(playDominoes_op mode player endpoints (DrawFromBoneyard 27 player endpoints dominoList) board) )
				;no tiles in boneyard, so game ended in draw
				( t
			  		(format t "~%DRAW!~%~%")
					0 ))
			)
			( t
				;indicate current player
				(format t "~%-------------------------------------------------~%~%") 
		 		(cond
					( (equal player 1)
						(format t "YOUR TURN~%") 
						(format t "---------~%~%") 
					)
					( t
						(format t "COMPUTER'S TURN~%") 
						(format t "---------------~%~%") 
					))
				;perform main output
				(dominoesOut dominoList 0)
				(dominoesOut dominoList 1)
				(dominoesOut dominoList 2)
				(OutputBoard board)
				(OutputEndpoints endpoints)
				
				(let*(
					;determine and play tile
					(playable (ExistPlayableTile dominoList 27 player endpoints))
					(combined (PlayTile mode player endpoints dominoList board (car playable) (car (cdr playable))))
					(newEndpoints (car combined))
					(newdominoList (car (cdr combined)))
					(newBoard (car (cdr (cdr combined)))))

					;repeat for next player
					(playDominoes_op mode (SwitchPlayer player) newEndpoints newdominoList newBoard) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  Pause 
;  PARAMETERS  :  
;  RETURNS     :  (output)
;  DESCRIPTION :  Waits until teh user hits a key and roessess enter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun Pause ()

;	 (format t "~%~%Press a key and hit enter to continue...~%")
;	 (read)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  SwitchPlayer
;  PARAMETERS  :  player
;  RETURNS     :
;  DESCRIPTION :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun SwitchPlayer (player)
	
	(cond
	 ( (equal player 1)
	 	2)
	 (t
		1) )
			)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  GetNumericInput
;  PARAMETERS  :  min, max, text
;  RETURNS     :  valid number
;  DESCRIPTION :  Accepts a range and prompt and returns a valid number
;                 within the range as entered by the user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GetNumericInput (min max text)

	(print text)
	(ValidateNumber min max (read))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  FUNCTION    :  ValidateNumber
;  PARAMETERS  :  min, max, value
;  RETURNS     :  valid number
;  DESCRIPTION :  Accepts a number until it is valid and within range.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ValidateNumber (min max value)

	(cond
		( (equal (numberp value) () )
			(format t "Not a valid number. Re-enter  : ")
			(ValidateNumber min max (read)) )
		( (< value min)
			(format t "Number too small. Re-enter  : ")
			(ValidateNumber min max (read)) )
		( (> value max)
			(format t "Number too large. Re-enter  : ")
			(ValidateNumber min max (read)) )
		( t
			value )
	)
)

(run)

