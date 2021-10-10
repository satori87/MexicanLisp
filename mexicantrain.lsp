;Load the other game files and start the game
(defun mexicanTrain ()	
	(loadModules)	
	(initGame)
	'"Thank you for playing. Goodbye."
)

(defun loadModules ()
	(load "boneyard.lsp")
	(load "utility.lsp")
	(load "round.lsp")
	(load "serialize.lsp")
	(load "deserialize.lsp")
	(load "game.lsp")
	(load "tile.lsp")
	(load "human.lsp")
	(load "computer.lsp")
	(load "player.lsp")
	(load "train.lsp")
	(load "help.lsp")
)

(mexicanTrain)