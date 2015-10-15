;;;; 1. DFS
;;; params:      TREE - list representation of a tree
;;; returns:     a list of terminal nodes in order visited
;;; description: DFS simply returns nil if the tree node is null and returns a
;;;              list containing the node if it is a single number. It then
;;;              recursively calls itself first on the closest node and then
;;;              on the rest of the tree.
(defun DFS (TREE)
  (cond ((null TREE) nil)
        ((atom TREE) (list TREE))
        (t (append (DFS (car TREE)) (DFS (cdr TREE))))))

;;;; 2. DFID
;;; params:      TREE - list representation of a tree
;;;              LIMIT - depth limit of search
;;; returns:     a list of terminal nodes in order visited
;;; description: Calls DFS-LIMITED for each limit from 0 to LIMIT. DFS-LIMITED
;;;              works the same way as DFS except as it goes down a level in 
;;;              tree it increments the LEVEL parameter. If LEVEL exceeds the 
;;;              limit the function returns nil.
(defun DFID (TREE LIMIT)
  (cond ((equal LIMIT 0) (DFS-LIMITED TREE 0 LIMIT))
        (t (append (DFID TREE (- LIMIT 1)) (DFS-LIMITED TREE 0 LIMIT)))))

(defun DFS-LIMITED (TREE LEVEL LIMIT)
  (cond ((null TREE) nil)
        ((> LEVEL LIMIT) nil)
        ((atom TREE) (list TREE))
        (t (append (DFS-LIMITED (car TREE) (+ LEVEL 1) LIMIT) (DFS-LIMITED (cdr TREE) LEVEL LIMIT)))))

; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond ((and (= (car s) 3) (= (cadr s) 3) (null (caddr s))) t) 
        (t NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (cond ((< (car s) m) NIL)
        ((< (cadr s) c) NIL)
        ((and (> (- (car s) m) 0) (< (- (car s) m) (- (cadr s) c))) NIL)
        ((and (> (- 3 (- (car s) m)) 0) (< (- 3 (- (car s) m)) (- 3 (- (cadr s) c)))) NIL)
        (t (list (list (- 3 (- (car s) m)) (- 3 (- (cadr s) c)) (not (caddr s)))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)
  (append (next-state s 2 0)
          (next-state s 1 0)
          (next-state s 1 1)
          (next-state s 0 1)
          (next-state s 0 2)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by MC-DFS (STATES). It returns T if S is a member of
; STATES and NIL otherwise.
(defun on-path (s states)
  (cond ((equal (length states) 0) NIL)
        ((equal (car states) s) t)
        (t (on-path s (cdr states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: the path
; from from the initial state to the current state (PATH), and the legal
; successor states to the last state on PATH (STATES). PATH is a first-in
; first-out list of states; that is, the first element is the initial state for
; the current search and the last element is the most recent state explored.
; MULT-DFS does a depth-first search on each element of STATES in turn. If any
; of those searches reaches the final state, MULT-DFS returns the complete path
; from the initial state to the goal state. Otherwise, it returns NIL.
(defun mult-dfs (states path)
  (cond ((equal (length states) 0) NIL)
        ((mc-dfs (first states) path) (mc-dfs (first states) path))
        (t (mult-dfs (rest states) path))))


; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) (append path (list s)))
        ((not (on-path s path)) (mult-dfs (succ-fn s) (append path (list s))))
        (t NIL)))
