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
;		n: 		index of element to get (1 for first)
;		lst:	list to get nth element of (assumed to be at least as large as n)
; Algorithm: Iterate recrusively through list, counting down n. When n is 0,
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: setNth
; Purpose: Returns a list with its nth element modified
; Parameters: 
;		n: 		index of element to set (1 for first)
;		lst:	list to set nth element of (assumed to be at least as large as n)
;		item:	the item to put at nth position
; Algorithm: Iterate recrusively through list, counting down n. When n is 0,
;			the desired element is the first in the remaining list. The new element
;			is inserted into the beginning of the rest of the list, which has the 
;			elements already checked inserted in the beginning as the recursive chain
;			ends
; Return Value: the list lst with nth element set to item
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setNth(n lst item)
	(cond
		( (<= n 1)
			(cons item (rest lst) ) )
		( t
			(cons (first lst) (setNth (- n 1) (rest lst) item ) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: reverseList
; Purpose: Simply returns lst reversed, with its members untouched
; Parameters: 
;		lst:	the list to reverse
; Algorithm: Iterate through list recursively, appending first element to the end of
;			new list
; Return Value: lst, reversed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reverseList(lst)
	(cond
		( (null lst)
			() )
		( t
			(append (reverseList (rest lst) ) (list (first lst) ) ) )

	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: remList
; Purpose: Returns a list with the first instance of an item removed from it
; Parameters: 
;		lst:	the lst to remove item from
;		item:	the item to remove from lst
; Algorithm: Recursively iterate through list until end of list, or item is encountered
;			equality is established with equal, not =, so this can be a more universal
;			function. When the first instance is encountered, return the rest of the list
;			without the item, and reconstruct the list with the already-checked items in front
;			as recursion ends
; Return Value: the list lst with first instance of item removed, if it exists
;				otherwise, returns lst			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getListLength
; Purpose: Returns the length of list
; Parameters: lst: the List to count
; Algorithm: Uses recursive counter to count list
; Return Value: The length of list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getListLength(lst)
	(listCounter 0 lst)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: listCounter
; Purpose: To count a list with argument n
; Parameters: n: the count so far, lst: the list
; Algorithm: Increment n every recursive step until list is empty
; Return Value: the length of list lst.
; Usage: Start with initial outside call arguing n as 0 to get accurate results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun listCounter(n lst)
	(cond
		( (null lst)
			n )
		(  t 
			(listCounter (+ n 1) (rest lst) )	)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getLast
; Purpose: Returns last element of a list
; Parameters: lst: the list
; Algorithm: Recursively iterate through the list until rest is null, indicating last
;	 		element. then return it.
; Return Value: last element of lst
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getLast(lst)
	(cond
		( (null (rest lst) )
			(first lst) )
		( t 
			(getLast (rest lst) ) )
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: listContains
; Purpose: Returns t or nil if lst contains at least once instance of item
; Parameters: lst: the list, item: the item to search for
; Algorithm: Recursively iterate through lst until item is found
; Return Value: t if found, nil otherwise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: andList
; Purpose: to get the result of two lists, ANDed together
; Parameters: lst and lst2, two lists to AND together cell by cell
; Algorithm: assumes lst and lst2 are same size
; 			returns a list same size as lst with result of
; 			lst[i] && lst2[i]
; 			if lst2 is longer, it returns a list as long as
; 			lst, containing the ANDs of the first n elements
; 			of lst and lst2, where n is the length of lst
; Return Value: a new list of length same as lst, containing the ANDed values of
;				lst and corresponding (parallel) value of lst2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun andList (lst lst2)
	(cond
		( (null lst)
			() )
		( t
			(cons (and (first lst) (first lst2) ) (andList lst lst2) ) )
			
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: allButLast
; Purpose: Returns all but the Last element of a list
; Parameters: lst, the list
; Algorithm: Return first element 
; Return Value: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun allButLast (lst)
	(reverseList (rest (reverseList lst) ) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: reverseEach
; Purpose: returns new list with each element reversed. The order of elements is the same,
;		but, assuming elements are also list, those lists are reversed. Can be combined with
;		reverseList to mirror a list of lists perfectly
; Parameters: lst
; Algorithm: Iterate through list, reversing all elements along way while preversing order
; Return Value: A list with all list sub-elements with their order reversed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: inputPath
; Purpose: Obtain a path/file from the console
; Parameters: N/A
; Algorithm: Make a pathname out of the console input.
; Return Value: a pathname type formed by user input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun inputPath ()
	(make-pathname :name (read-line) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: openValidFile
; Purpose: To obtain and open a valid file from user input
; Parameters: N/A
; Algorithm: Use inputPath to obtain a pathname type, use probe-file
;			to verify its integrity. Do not open until a valid file is found
; Return Value: A stream resulting from successfully opening file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: openFileForSave
; Purpose: Obtains a pathname from user and opens stream for writing
; Parameters: N/A
; Algorithm: Open the path for output. If file exists already, remove existing file first
; Return Value: Open stream for writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun openFileForSave ()
	(princ "Enter a filename to open for save") (terpri)
	(open (inputPath) :direction :output :if-exists :supersede)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;						LIST SHUFFLING FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getShuffledList, shuffleList
; Purpose: Returns a shuffled copy of lst
; Parameters: lst to shuffle
; Algorithm:  Using length of lst, create a new list of numbers 1 through lst length,
;				in random order. Then, use that list as indexes pointing to cells of
;				rawBoneyard, to generate a new, shuffled list
; Return Value: 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getRandomOrder, randomOrder
; Purpose: Create list of random numbers 1 through max, without duplicates
; Parameters: max - number of elements
; Algorithm: use getUnusedRandomIndex to iteratively fill the list
; Return Value: list of random numbers 1 through max, without duplicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getUnusedRandomIndex
; Purpose: return a number 1-max not inside list
; Parameters: n: list counter, the list of random orders so far, max value
; Algorithm: Simply generate random numbers between 1 and max, using recursive loop,
;			 until one is generated that is not in the list. then return it.
; Return Value: an integer between 1 and max that is not already found in order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: getValidNumber
; Purpose: Obtains a valid number between min and max from the user
; Parameters: min value, max value, prompt to display
; Return Value: a valid number between min and max
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getValidNumber (min max prompt)
	(princ prompt) (terpri)
	(promptValidNumber min max)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: promptValidNumber
; Purpose: prompt for valid number between min and max
; Parameters: min value, max value
; Algorithm: 
; Return Value: a value user input between min and max
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun promptValidNumber (min max)
	(format t "Please enter a number between ~d and ~d~%" min max)
	(verifyNumberInRange min max (read) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: verifyNumberInRange
; Purpose: verifies console input is a valid number between min and max
; Parameters: min, max, console input
; Algorithm: 
; Return Value: a valid number between min and max
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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