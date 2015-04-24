#|
 | Program Name: Fucntional Programming in Lisp Missionaries and Cannibals Puzzle
 | Authors: Dylan Geyer, Chrissy Sorensen
 | Class: CSC 461 Programming Languages
 | Instructor: Dr. J. Weiss
 | Date: Apr 27 2015
 |
 | Usage: clisp m-c m c (m=number of missionaries, c=number of canibals)
 |
 |
 |
 |
 | |#

; global variables
(defvar M*)
(defvar C*)
(defvar Visited*)
(defvar SolutionPath*)

;load BFS/DFS routines from external file
;(load 'search)

#|
 | Function Name: Main()
 | Description: The main function where the program starts. Checks for 
 | three command line arguments, if so, stores the values into the given
 | for number of missionaries and number of cannibals into respected
 | globl variables and calls funtion "m-c" passing in the values.
 |
 | If invalid, the program prints out a usage statement
 | to the console.
 |
 | Parameters: None.
 |
 |#


(defun main()
	(cond
		((= (length *args*) 2)	;condition
			(setf M* (parse-integer (car *args*)) C* (parse-integer (cadr *args*))) ;e1
			(m-c M* C*)	;e2
		)
		(t
			(format t "Generalized Missionaries and Cannibals Problem~%")
			(format t "----------- ------------ --- --------- -------~%")
			(format t "Usage: (Missionaries M C)~% ~C M - Missionaries~% ~C C - Cannibals" #\tab #\tab)
		)
	)
	
)

#|
 | Function Name: m-c
 | 
 | Description: Checks the conditions of the values. If there is more
 | cannibals than missionaries, the program prints out an error to the console
 | and returns. If not, it sets the global variables for missionaries and
 | cannibals to the values passed in, sets the Visted list to nil, sets the
 | SoulutionPath to nil. It calls start-state and then passes the return into
 | recursive-dfs function. Once that returns, the function calls the print-
 | solution to print the solution path.
 |
 | Parameters: 
 | M - number of missionaries
 | C - number of cannibals
 | 
 |#
;Assuming all Missionaries/Cannibals start on left bank with canoe
(defun m-c (M C)
	;Check to make sure the start state is valid
	(cond
		((and (> C M)(> M 0)) 	;Condition 1 (There are more cannibals than missionaries
			(format t "There are too many cannibals at the start. No solution.")
			(return-from m-c (values))
		)
		(t	;Valid start state, initialize global variables
			(setf M* M)
			(setf C* C)
			(setf Visited* nil)
			(setf SolutionPath* nil)
		)
	)
	
	;Solve the Missionaries & Cannibals problem using recursive DFS
	(recursive-dfs (start-state))
	
	;Now print out the solution path in a useful way
	(print-solution)
	(return-from m-c (values))
)

#|
 | Function Name: start-state
 | 
 | Description: Sets the start state to empty missionaries and cannibals on the
 | right side.
 | 
 | Parameters: none
 |
 |#

(defun start-state () '(0 0 L))

#|
 | Function Name: goal-state
 | 
 | Description:
 | 
 | Parameters:
 |
 |#
(defun goal-state (state)
	(cond
		((equal state (list M* C* 'R))
			(return-from goal-state t)
		)
		(t
			(return-from goal-state nil)
		)
	)
)

#|
 | Function Name: recursive-dfs
 | Description:
 | Parameters:
 |
 |#
(defun recursive-dfs (state)
	;Set state as visited
	(push state Visited*)
	
	;Push state to SolutionPath
	(push state SolutionPath*)

	
		
	;Generate all successors to 'state'
	(setf succs (generate-successors state))
		;For all successors to 'state'
			;Recursively call DFS
	(loop for next-state in succs do 
		(recursive-dfs next-state)
	)
		
	;If (car SolutionPath) == goal-state
		;return-from recursive-dfs
	(if (goal-state (car SolutionPath*)) (return-from recursive-dfs) t)
	;Pop SolutionPath
	(pop SolutionPath*)
)

#|
 | Function Name: print-solution
 | Description:
 | Parameters"
 |
 |#
(defun print-solution()
	(format t "~d Missionaries and ~d Canibals: ~%" M* C* )
	(format t  "left bank~Cright bank~Ccanoe~Clast move ~%" #\tab #\tab #\tab)
	(format t "---------~C----------~C-----~C---------~%" #\tab #\tab #\tab )
	(setf SolutionPath* (reverse SolutionPath*))
	(setf prev-state (car SolutionPath*) )
        (loop for next-state in SolutionPath* do
                (print-steps next-state prev-state)
		(setf prev-state next-state)
        )
)

#|
 | Function Name: print-steps
 | Description:
 | Parameters:
 |
 |#

(defun print-steps( state prev-state )
       ;Create a local scope so we can use some temporary variables
        (let ((m-left (- M* (nth 0 state)))
                  (m-right (nth 0 state))
                  (c-left (- C* (nth 1 state)))
                  (c-right (nth 1 state))
		  (m-prev-right (nth 0 prev-state))
		  (c-prev-right (nth 1 prev-state))
                  (canoe (if (equal 'L (nth 2 state)) 'left 'right))   ;Gives opposite bank
                  )
		(format t "~d M, ~d C ~C~d M, ~d C ~C~s~C" m-left c-left #\tab m-right c-right #\tab canoe #\tab)
		(cond
			((equal state prev-state) (format t "start position~%") nil)
			(t
				(let ((dif-m nil)
					(dif-c nil))
					(format t "move")
					(setf dif-m (abs (- m-right m-prev-right)))
					(setf dif-c (abs (- c-right c-prev-right)))
					(format t " ~d M," dif-m)
					(format t " ~d C" dif-c)
					(if (equal canoe 'left) (format t " right to left~%") (format t " left to right~%"))
				)

			)
			
		)
        )
	
)

#|
 | Function Name:
 | Description:
 | Parameters:
 |
 |#
(defun generate-successors (state)
	;Create a local scope so we can use some temporary variables
	(let ((m-left (- M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- C* (nth 1 state)))
		  (c-right (nth 1 state))
		  (canoe (if (equal 'R (nth 2 state)) 'L 'R))	;Gives opposite bank
		  (next-state nil)
		  (successors nil))
		
		;Move 1 Cannibal
		(setf next-state (move-cannibals state 1))
		(if next-state (push next-state successors) nil)
		
		;Move 1 Missionary
		(setf next-state (move-missionaries state 1))
		(if next-state (push next-state successors) nil)
		
		;Move 1 Cannibal, 1 Missionary
		(setf next-state (move-mc state 1))
		(if next-state (push next-state successors) nil)
		
		;Move 2 Cannibals
		(setf next-state (move-cannibals state 2))
		(if next-state (push next-state successors) nil)
		
		;Move 2 Missionaries
		(setf next-state (move-missionaries state 2))
		(if next-state (push next-state successors) nil)
		
		;return list of successors with no duplicates
		(remove-duplicates successors :test #'equal)
	)
)

#|
 | Function Name: move-cannibals
 | Description:
 | Parameters:
 |
 |#

;Returns the state resulting from moving 1 cannibal
(defun move-cannibals (state n)
	(let ((m-left (- M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- C* (nth 1 state)))
		  (c-right (nth 1 state))
		  (canoe (if (equal 'R (nth 2 state)) 'L 'R))
		  (next-state nil))
		(cond
			((equal 'R (nth 2 state))	;If canoe is on right bank
				;Move cannibal from RIGHT -> LEFT
				(setf next-state (list m-right (- c-right n) canoe))
			)
			
			((equal 'L (nth 2 state))
				;Move cannibal from LEFT TO RIGHT
				(setf next-state (list m-right (+ c-right n) canoe))
			)
		)
		(cond
			((valid-state next-state)
				;(print next-state)
				;(print "is valid")
				(return-from move-cannibals next-state)
			)
			(t
				;(print next-state)
				;(print "is invalid")
				(return-from move-cannibals nil)
			)
		)
	)
)

#|
 | Function Name: move-missionaries
 | Description:
 | Parameters:
 |
 |#

;Returns the state resulting from moving 1 missionary
(defun move-missionaries (state n)
	(let ((m-left (- M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- C* (nth 1 state)))
		  (c-right (nth 1 state))
		  (canoe (if (equal 'R (nth 2 state)) 'L 'R))	;Gives opposite bank
		  (next-state nil))
		(cond
			((equal 'R (nth 2 state))	;If canoe is on right bank
				;Move cannibal from RIGHT -> LEFT
				(setf next-state (list (- m-right n) c-right canoe))
			)
			
			((equal 'L (nth 2 state))
				;Move cannibal from LEFT TO RIGHT
				(setf next-state (list (+ m-right n) c-right canoe))
			)
		)
		(cond
			;Check this new state to make sure the missionaries are safe
			((valid-state next-state)
				;(print next-state)
				;(print "is valid")
				(return-from move-missionaries next-state)
			)
			(t
				;(print next-state)
				;(print "is invalid")
				(return-from move-missionaries nil)
			)
		)
	)
)

#|
 | Function: move-mc
 | Description:
 | Parameters:
 |
 |#

;Returns the state resulting from moving missionaries and cannibals
(defun move-mc (state n)
	(let ((m-left (- M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- C* (nth 1 state)))
		  (c-right (nth 1 state))
		  (canoe (if (equal 'R (nth 2 state)) 'L 'R))	;Gives opposite bank
		  (next-state nil))
		(cond
			((equal 'R (nth 2 state))	;If canoe is on right bank
				;Move cannibal from RIGHT -> LEFT
				(setf next-state (list (- m-right n) (- c-right n) canoe))
			)
			
			((equal 'L (nth 2 state))
				;Move cannibal from LEFT TO RIGHT
				(setf next-state (list (+ m-right n) (+ c-right n) canoe))
			)
		)
		(cond
			;Check this new state to make sure the missionaries are safe
			((valid-state next-state)
				;(print next-state)
				;(print "is valid")
				(return-from move-mc next-state)
			)
			(t
				;(print next-state)
				;(print "is invalid")
				(return-from move-mc nil)
			)
		)
	)
)

#|
 | Function Name: valid-state
 | Description:
 | Parameters:
 |
 |#

;Returns true if Missionaries are safe, nil if they get eaten
(defun valid-state (state)
	
	;Set up all the local variables needed for easy checking
	(let ((m-left (- M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- C* (nth 1 state)))
		  (c-right (nth 1 state)))
		
		(cond
			;If this state has been visited before return nil
			((member state Visited* :test #'equal)
				(return-from valid-state nil)
			)
			;If this state has too many cannibals on the left side, return nil
			((and (> c-left m-left) (> m-left 0))
				(return-from valid-state nil)

			)
			;If this state has too many cannibals on the right side, return nil
			((and (> c-right m-right) (> m-right 0))
				(return-from valid-state nil)
				
			)
			;If any # of Missionaries/Cannibals go negative, its invalid
			((or (< c-left 0 ) (< c-right 0) (< m-left 0) (< m-right 0))
				(return-from valid-state nil)
			)
			;Else return true
			(t
				(return-from valid-state t)
			)
		)
	)
)

(main)
