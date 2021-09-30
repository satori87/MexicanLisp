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

(defun printListLn (label lst)
	(cond
		( (null lst)
			(format t "~d" label) (terpri) )
		( t
			(format t "~d~d" label lst) (terpri) )
	)	
)

(defun printList (label lst)
	(cond
		( (null lst)
			(format t "~d" label) )
		( t
			(format t "~d~d" label lst) )
	)	
)

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

(defun removeFromList(lst item)
	(cond
		( (null lst)
			() )
		( (= (first lst) item)
			(rest lst) )
		( t
			(cons (first lst) (removeFromList (rest lst) item ) ) )
	)
)

(defun setNth(lst n item)
	(cond
		( (<= n 1)
			(cons item (rest lst) ) )
		( t
			(cons (first lst) (setNth (rest lst) (- n 1) item ) ) )
	)
)

(defun getListLength(lst)
	(listCounter 0 lst)
)

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

(defun getShuffledList(lst)
	(shuffleList 0 (getRandomOrder (getListLength lst) ) () (rawBoneyard) )
)

(defun shuffleList(n order lst olst)
	(cond
		( (>= n (getListLength olst) )
			lst )
		( t
			(shuffleList (+ n 1) order (cons (getNth (getNth n order) olst ) lst) olst ) )
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
	(let( ( rand (random max) ) )
		(cond
			( (null (listContains order rand) )
				rand )
			( t
				(getUnusedRandomIndex n order max) )
		)
	)
)