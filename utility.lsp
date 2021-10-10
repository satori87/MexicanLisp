;     ************************************************************
;     * Name:  Michael Whitlock                                  *
;     * Project:  Mexican Train (LISP)                           *
;     * Class:  OPL Fall 2021                                    *
;     * Date:  10/10/21                                          *
;     ************************************************************
;
;     utility.lsp Holds generic utiltiy functions that are not directly 
;	  related to Mexican Train
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;						GENERAL LIST FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: printListLn
; Purpose: Prints out a list along with custom label, followed by newline
; Parameters:
;		label:	text to print before list
;		lst:	the list to print out
; Algorithm: If list is empty,
;				print label and new line
;			 else
;				print label followed by list and new line
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun printListLn (label lst)
	(cond
		( (null lst)
			(format t "~d" label) (terpri) )
		( t
			(format t "~d~d" label lst) (terpri) )
	)	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: printList
; Purpose: Prints out a list along with custom label
; Parameters:
;		label:	text to print before list
;		lst:	the list to print out
; Algorithm: If list is empty,
;				print label
;			 else
;				print label followed by list
; Return Value: N/A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun printList (label lst)
	(cond
		( (null lst)
			(format t "~d" label) )
		( t
			(format t "~d~d" label lst) )
	)	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getNth
; Purpose: Returns the nth member of a list
; Parameters: 
;		n: 		index of element to get
;		lst:	list to get nth element of (assumed to be at least as large as n)
; Algorithm: Iterate recrusively through list from, counting down n. When n is 0,
;			the desired element is the first in the remaining list
; Return Value: The nth element of lst
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getNth(n lst)
	(cond
		( (<= n 1)
			(first lst) )
		(  t 
			(getNth (- n 1) (rest lst) )	)
	)
)

(defun reverseList(lst)
	(cond
		( (null lst)
			() )
		( t
			(append (reverseList (rest lst) ) (list (first lst) ) ) )

	)
)

(defun remList(lst item)
	(cond
		( (null lst)
			() )
		( (equal (first lst) item)
			(rest lst) )
		( t
			(cons (first lst) (remList (rest lst) item ) ) )
	)
)

(defun setNth(n lst item)
	(cond
		( (<= n 1)
			(cons item (rest lst) ) )
		( t
			(cons (first lst) (setNth (- n 1) (rest lst) item ) ) )
	)
)

(defun getListLength(lst)
	(listCounter 0 lst)
)

;for use with getListLength
(defun listCounter(n lst)
	(cond
		( (null lst)
			n )
		(  t 
			(listCounter (+ n 1) (rest lst) )	)
	)
)

;get last element of a list
(defun getLast(lst)
	(cond
		( (null (rest lst) )
			(first lst) )
		( t 
			(getLast (rest lst) ) )
	)
)

;returns t or nil if the list contains the item
(defun listContains(lst item)
	(cond
		( (null lst)
			() )
		( (equal (first lst) item)
			t )
		( t
			(listContains (rest lst) item ) )

	)
)

; assumes lst and lst2 are same size
; returns a list same size as lst with result of
; lst[i] && lst2[i]
; if lst2 is longer, it returns a list as long as
; lst, containing the ANDs of the first n elements
; of lst and lst2, where n is the length of lst
(defun andList (lst lst2)
	(cond
		( (null lst)
			() )
		( t
			(cons (and (first lst) (first lst2) ) (andList lst lst2) ) )
			
	)
)

; returns all but the last element of a list
(defun allButLast (lst)
	(reverseList (rest (reverseList lst) ) )
)

; doesnt reverse the list itself, but the order of the first nesting
; of lists (non recursive)
(defun reverseEach (lst)
	(cond
		( (null lst)
			() )
		( (listp (first lst) )
			(cons (reverseList (first lst) ) (reverseEach (rest lst) ) ) )
		( t
			(cons (first lst) (reverseEach (rest lst) ) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;						FILE/PATH UTILITY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inputPath ()
	(make-pathname :directory "lisp" :name (read-line) )
)

(defun openValidFile ()
	(princ "Enter a valid filename to open for load") (terpri)
	(let ( (path (inputPath) ) )
		(cond
			( (null (probe-file path) )
				(openValidFile) )
			( t
				(open path) )
		)
	)
)

(defun openFileForSave ()
	(princ "Enter a filename to open for save") (terpri)
	(open (inputPath) :direction :output :if-exists :supersede)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;						LIST SHUFFLING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getShuffledList(lst)
	(shuffleList (getRandomOrder (getListLength lst) ) (rawBoneyard) )
)

(defun shuffleList(order lst)
	(cond
		( (null order)
			() )
		( t
			(cons (getNth (first order) lst) (shuffleList (rest order) lst) ) )
	)
)

(defun getRandomOrder(max)
	(randomOrder () max )
)

;return a list of the first max numbers in random order
(defun randomOrder(order max)
	(cond
		( (= (getListLength order) max)
			order )
		( t 
			(randomOrder (cons (getUnusedRandomIndex 0 order max) order) max ) )
	)
)

;return a number 1-max not inside list
(defun getUnusedRandomIndex(n order max)
	(let( ( rand (+ 1 (random max) ) ) )
		(cond
			( (null (listContains order rand) )
				rand )
			( t
				(getUnusedRandomIndex n order max) )
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;						NUMERIC INPUT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getValidNumber (min max prompt)
	(princ prompt) (terpri)
	(promptValidNumber min max)
)

(defun promptValidNumber (min max)
	(format t "Please enter a number between ~d and ~d~%" min max)
	(verifyNumberInRange min max (read) )
)

(defun verifyNumberInRange(min max consoleInput)
	(cond
		( (equal (numberp consoleInput) () )
			(promptValidNumber min max) )
		( (< consoleInput min)
			(promptValidNumber min max) )
		( (> consoleInput max)
			(promptValidNumber min max) )
		( t
			consoleInput )
	)
)