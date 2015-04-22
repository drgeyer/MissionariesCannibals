; global variables
(defvar *M*)
(defvar *C*)
(defvar *Visited*)
(defvar *SolutionPath*)

;load BFS/DFS routines from external file
;(load 'search)

(defun main()
	(cond
		((= (length *args*) 2)	;condition
			(setf *M* (parse-integer (car *args*)) *C* (parse-integer (cadr *args*))) ;e1
			(Missionaries *M* *C*)	;e2
		)
		(t
			(print "Generalized Missionaries and Cannibals Problem")
			(print "----------- ------------ --- --------- -------")
			(print "Usage: (Missionaries M C)")
			(print "		M - Missionaries")
			(print "		C - Cannibals")
		)
	)
)

;Assuming all Missionaries/Cannibals start on left bank with canoe
(defun Missionaries (M C)
	;Check to make sure the start state is valid
	(cond
		((> C M)	;Condition 1 (There are more cannibals than missionaries
			(print "There are too many cannibals at the start. No solution.")
			(return-from Missionaries nil)
		)
		(t	;Valid start state, initialize global variables
			(setf *M* M)
			(setf *C* C)
			(setf *Visited* nil)
			(setf *SolutionPath* nil)
		)
	)
	
	;Solve the Missionaries & Cannibals problem using recursive DFS
	(recursive-dfs (start-state))
	
	;Now print out the solution path in a useful way
	(print-solution)
	
)

(defun start-state () '(0 0 L))

(defun goal-state (state)
	(cond
		((equal state (list *M* *C* 'R))
			;(print "SOLUTION FOUND!!!")
			(return-from goal-state t)
		)
		(t
			(return-from goal-state nil)
		)
	)
)

(defun recursive-dfs (state)
	;Set state as visited
	(push state *Visited*)
	
	;Push state to SolutionPath
	(push state *SolutionPath*)

	
		
	;Generate all successors to 'state'
	(setf succs (generate-successors state))
		;For all successors to 'state'
			;Recursively call DFS
	(loop for next-state in succs do 
		(recursive-dfs next-state)
	)
		
	;If (car SolutionPath) == goal-state
		;return-from recursive-dfs
	(if (goal-state (car *SolutionPath*)) (return-from recursive-dfs) t)
	;Pop SolutionPath
	(pop *SolutionPath*)
)

(defun print-solution()
	(print *SolutionPath*)
)

(defun generate-successors (state)
	;Create a local scope so we can use some temporary variables
	(let ((m-left (- *M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- *C* (nth 1 state)))
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

;Returns the state resulting from moving 1 cannibal
(defun move-cannibals (state n)
	(let ((m-left (- *M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- *C* (nth 1 state)))
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

;Returns the state resulting from moving 1 missionary
(defun move-missionaries (state n)
	(let ((m-left (- *M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- *C* (nth 1 state)))
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

;Returns the state resulting from moving missionaries and cannibals
(defun move-mc (state n)
	(let ((m-left (- *M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- *C* (nth 1 state)))
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

;Returns true if Missionaries are safe, nil if they get eaten
(defun valid-state (state)
	
	;Set up all the local variables needed for easy checking
	(let ((m-left (- *M* (nth 0 state)))
		  (m-right (nth 0 state))
		  (c-left (- *C* (nth 1 state)))
		  (c-right (nth 1 state)))
		
		(cond
			;If this state has been visited before return nil
			((member state *Visited* :test #'equal)
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
