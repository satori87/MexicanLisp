;Load the other game files and start the game
(defun mexicanTrain ()
	;setq is necesarry in order to set environment variable pretty-print to off
	(setq *print-pretty* nil)
	(loadModules)	
	(initGame)
	'"Thank you for playing. Goodbye."
)

;load game modules
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