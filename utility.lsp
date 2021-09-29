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

(defun printList (label list)
	(cond
		( (null list)
			(format t "~d" label) (terpri) )
		( t
			(format t "~d~d" label list) (terpri) )
	)	
)

(defun getNth(n list)
	(cond
		( (<= n 1)
			(first list) )
		(  t 
			(getNth (- n 1) (rest list) )	)
	)
)

(defun getListLength(list)
	(listCounter 0 list)
)

(defun listCounter(n list)
	(cond
		( (null list)
			n )
		(  t 
			(listCounter (+ n 1) (rest list) )	)
	)
)

;get last element of a list
(defun getLast(list)
	(cond
		( (= (getListLength list) 1) 
			list )
		( t 
			(getLast (rest list) ) )
	)
)

;returns t or nil if the list contains the item
(defun listContains(list item)
	(cond
		( (null list)
			() )
		( (equal (first list) item)
			t )
		( t
			(listContains (rest list) item ) )

	)
)