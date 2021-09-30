
(defun setRoundNumber(game roundNumber)
	(setNth game 1 roundNumber)
)

(defun setComputerScore(game computerScore)
	(setNth game 2 computerScore)
)

(defun setComputerHand(game computerHand)
	(setNth game 3 computerHand)
)

(defun setComputerTrain(game computerTrain)
	(setNth game 4 computerTrain)
)

(defun setHumanScore(game humanScore)
	(setNth game 5 humanScore)
)

(defun setHumanHand(game humanHand)
	(setNth game 6 humanHand)
)

(defun setHumanTrain(game humanTrain)
	(setNth game 7 humanTrain)
)

(defun setMexicanTrain(game mexicanTrain)
	(setNth game 8 mexicanTrain)
)

(defun setBoneyard(game boneyard)
	(setNth game 9 boneyard)
)

(defun setNextPlayer(game nextPlayer)
	(setNth game 10 nextPlayer)
)

(defun setComputerPassed(game computerPassed)
	(setNth game 11 (list computerPassed (getNth 2 (getNth 11 game) ) ) )
)

(defun setHumanPassed(game humanPassed)
	(setNth game 11 (list (getNth 1 (getNth 11 game) ) humanPassed ) )
)