(defun getRoundNumber(game)
	(getNth 1 game)
)

(defun getComputerScore(game)
	(getNth 2 game)
)

(defun getComputerHand(game)
	(getNth 3 game)
)

(defun getComputerTrain(game)
	(getNth 4 game)
)

(defun getHumanScore(game)
	(getNth 5 game)
)

(defun getHumanHand(game)
	(getNth 6 game)
)

(defun getHumanTrain(game)
	(getNth 7 game)
)

(defun getMexicanTrain(game)
	(getNth 8 game)
)

(defun getBoneyard(game)
	(getNth 9 game)
)

(defun getNextPlayer(game)
	(getNth 10 game)
)

(defun serializedGame1()
	'(
		1
		0
		( (1 3) (5 5) (0 3) (0 8) (1 9) (2 6) (3 9) (3 8) (6 6) (2 5) (5 6) (6 9) (3 6) (1 6) )
		( M (7 8) (8 9) (9 9) )
		0 
		( (0 7) (1 2) (6 7) (2 9) (1 7) (0 6) (5 7) (8 8) (3 7) (4 4) (7 7) (6 8) )
		( (9 9) (9 5) (5 3) )
		( (9 0) (0 0) (0 4) )
		( (0 1) (4 9) (0 9) (4 6) (2 4) (4 8) (0 2) (1 5) (4 5) (2 2) (0 5) (4 7) (1 1) (5 8) (3 4) (1 4) (2 8) (7 9) (1 8) (2 3) (3 3) (2 7) )
		Human
	)
)

(defun serializedGame2()
	'(
		1
		0  
		( (0 7) (1 2) (6 7) (2 9) (7 9) (1 7) (0 6) (5 7) (8 8) (3 7) (4 4) (7 7) )
		( (3 5) (5 9) (9 9) )
		0 
		( (1 3) (5 5) (0 3) (0 8) (1 9) (2 6) (3 9) (3 8) (1 5) (2 5) (5 6) (6 9) (3 6) (1 6) )
		( (9 9) (9 8) (8 7) M )
		( (9 0) (0 0) (0 4) )
		( (0 1) (0 9) (3 3) (4 9) (4 6) (2 4) (4 8) (0 2) (4 5) (2 2) (0 5) (4 7) (1 1) (5 8) (3 4) (1 4) (2 8) (6 8) (1 8) (2 3) (6 6) (2 7) )
		Computer
	)
)

(defun serializedGame3()
	'(
		2
		0
		( (2 7) )
		( (0 0) (0 5) (5 9) (9 2) (2 6) (6 6) (6 0) (0 2) (2 5) (5 1) (1 1) (1 2) (2 8) (8 8) )
		13 
		( (4 4) (1 9) )
		( (8 8) (8 5) (5 5) (5 3) (3 6) (6 8) (8 9) (9 4) )
		( (8 0) (0 3) (3 3) (3 4) (4 5) (5 7) (7 1) (1 4) (4 0) (0 9) (9 9) (9 6) (6 1) (1 8) (8 7) (7 7) (7 0) (0 1) (1 3) (3 8) (8 4) (4 7) (7 9) (9 3) (3 2) (2 2) (2 4) (4 6) (6 7) )
		( (3 7) (5 6) )
		Human
	)
)