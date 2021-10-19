;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     mexicantrain.lsp is the entry point for the game
;

;Interpreter specific stuff for demonstration
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;Load the other game files and start the game
(defun mexicanTrain ()
	;Interpreter specific stuff for demonstration
	(setq *load-verbose* nil)
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