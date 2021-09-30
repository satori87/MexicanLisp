(defun mexicanTrain()
	(loadModules)
	(initGame)
	'"Thank you for playing. Goodbye."
)

(defun loadModules()
	(load "C:/lisp/boneyard.lsp" :verbose t)
	(load "C:/lisp/utility.lsp" :verbose t)
	(load "C:/lisp/round.lsp" :verbose t)
	(load "C:/lisp/serialize.lsp" :verbose t)
	(load "C:/lisp/deserialize.lsp" :verbose t)
	(load "C:/lisp/game.lsp" :verbose t)
)

(mexicanTrain)