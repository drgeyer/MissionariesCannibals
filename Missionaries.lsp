#|
 | Program Name: Fucntional Programming in Lisp Missionaries and Cannibals Puzzle
 | Authors: Dylan Geyer, Chrissy Sorensen
 | Class: CSC 461 Programming Languages
 | Instructor: Dr. J. Weiss
 | Date: Apr 27 2015
 |
 | Descriptions:	This program solves the generalized verison of the missionaries and cannibals
 |					Problem and allows the user to specify how many Missionaries / Cannibals start
 |					on the left bank.
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
 |
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
 | Returns: None.
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
 | Returns:
 | values - prevents NIL from being printed
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
 | Description: Sets the start state to Zero missionaries and cannibals on the
 | right side.
 | 
 | Parameters: none
 |
 | Returns:
 | List (0 0 L) meaning 0 Cannibals, 0 Missionaries, Canoe on LEFT bank
 |#
(defun start-state () '(0 0 L))

#|
 | Function Name: goal-state
 | 
 | Description: This function determines whether or not the passed in State
 |				is equivalent to the Goal-State. It returns true if it is,
 |				and NIL if it is not.
 | 
 | Parameters:
 |	state - state to be compared to GOAL
 |
 | Returns:
 | t - state == goal-state
 | NIL - state != goal-state
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
 | Function Name:	recursive-dfs
 |
 | Description:	This functions performs a recursive depth first search
 |				on the graph generated using the Missionaries and Cannibals
 |				rules. To prevent loops, this dfs pushes visited states onto
 |				a visited list, and states on the solution path are saved on
 |				a solution path list.
 |
 | Parameters:	
 |  state - current state
 |
 | Returns:	
 |  None.
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
 | Function Name:	print-solution
 |
 | Description:	Once the recursive-dfs has finished runnign and the
 |				SolutionPath list has been populated, we need to print
 |				this path out in a way the user can understand. We begin
 |				by displaying the number of Missionaries/Cannibals on the
 |				Left bank, then the Right bank. Then we tell how many Missionaries
 |				and Cannibals moved in which direction to get from the previous
 |				state to the current state. 
 |
 | Parameters:
 |	None.
 |
 | Returns:	
 |	None.
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
 | Function Name:	print-steps
 |
 | Description:	This function prints an individual state along the solution path.
 |				it is responsible for determining how many C and M are on each bank
 |				and the difference between the C and M from the previous and current
 |				state.
 |
 | Parameters:
 |	state - current state along the solution path
 |	prev-state - previous state along the solution path
 |
 | Returns:	
 |	None.
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
 | Function Name:	generate-successors
 |
 | Description:	This function will generate all possible next states given
 |				a current state. It will only return states that are valid
 |				i.e. havent been visited before, and the missionaries are safe.
 |
 | Parameters:
 |	state - current state to take 1 step from
 |
 | Returns:	
 |	successors - list of successor states that we can go to from current state
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
		(if next-state (push next-state successors) nil) ;if next-state is non-nil push it to list of successors
		
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
 | Function Name:	move-cannibals
 |
 | Description:	This function moves n cannibals from one bank to the other depending
 |				on where the canoe is in state. For this program n is limited to 1 or
 |				2, but can actually be any number.
 |
 | Parameters:	
 |	state - current state of the missionaries, cannibals, canoe
 |	n - number of cannibals to move from canoe-side to noncanoe-side
 |
 | Returns:	
 |	next-state - result of moving n cannibals to the side that doesnt have the canoe
 |#
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
 | Function Name:	move-missionaries
 |
 | Description:	This function moves n missionaries from one bank to the other depending
 |				on where the canoe is in state. For this program n is limited to 1 or
 |				2, but can actually be any number.
 |
 | Parameters:	
 |	state - current state of the missionaries, cannibals, canoe
 |	n - number of missionaries to move from canoe-side to noncanoe-side
 |
 | Returns:	
 |	next-state - result of moving n missionaries to the side that doesnt have the canoe
 |#
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
 | Function Name:	move-mc
 |
 | Description:	This function moves n Missionaries AND Cannibals from the side with the canoe
 |				to the side without the canoe based on state. This program will only ever call
 |				this function with n = 1, but this can be any number.
 |
 | Parameters:
 |	state - contains number of missionaries/cannibals  on right bank, location of canoe
 |
 | Returns:	
 |	next-state - result of moving n cannibals from side with canoe to side without canoe
 |#
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
 | Function Name:	valid-state
 |
 | Description:	This function just determines whether or not a given state
 |				is valid, i.e. The number of cannibals on a bank is never
 |				greater than the missionaries, the state has not already
 |				been visited.
 |
 | Parameters:
 |	state - the state to be tested for validity
 |
 | Returns:	
 |	t - state is valid, we may move to it
 |	NIL - state is invalid for one of many reasons.
 |#
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

;Now that all functions are defined, call main
(main)
