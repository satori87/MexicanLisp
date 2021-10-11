;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     mexicantrain.lsp is the entry point for the game
;

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
	(load "boneyard.lsp" :verbose nil)
	(load "utility.lsp" :verbose nil)
	(load "round.lsp" :verbose nil)
	(load "serialize.lsp" :verbose nil)
	(load "deserialize.lsp" :verbose nil)
	(load "game.lsp" :verbose nil)
	(load "tile.lsp" :verbose nil)
	(load "human.lsp" :verbose nil)
	(load "computer.lsp" :verbose nil)
	(load "player.lsp" :verbose nil)
	(load "train.lsp" :verbose nil)
	(load "help.lsp" :verbose nil)
)

(mexicanTrain)