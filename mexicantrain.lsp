
(defun mexicanTrain ()	
	(loadModules)	
	(initGame)
	'"Thank you for playing. Goodbye."
)

(defun loadModules ()
	(load "C:/lisp/boneyard.lsp")
	(load "C:/lisp/utility.lsp")
	(load "C:/lisp/round.lsp")
	(load "C:/lisp/serialize.lsp")
	(load "C:/lisp/deserialize.lsp")
	(load "C:/lisp/game.lsp")
	(load "C:/lisp/tile.lsp")
	(load "C:/lisp/human.lsp")
	(load "C:/lisp/computer.lsp")
	(load "C:/lisp/player.lsp")
	(load "C:/lisp/train.lsp")
	(load "C:/lisp/help.lsp")
)

(mexicanTrain)