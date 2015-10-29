;
; CS161 HW3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp"))

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star))

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank))

(defun isWall (v)
  (= v wall))

(defun isBox (v)
  (= v box))

(defun isKeeper (v)
  (= v keeper))

(defun isStar (v)
  (= v star))

(defun isBoxStar (v)
  (= v boxstar))

(defun isKeeperStar (v)
  (= v keeperstar))

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
  (t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
         col
       (getKeeperColumn (cdr r) (+ col 1))
       );end if
     );end t
  );end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
  (t (let ((x (getKeeperColumn (car s) 0)))
       (if x
     ;keeper is in this row
     (list x row)
     ;otherwise move on
     (getKeeperPosition (cdr s) (+ row 1))
     );end if
         );end let
   );end t
  );end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
  (t (let ((cur (car L))
     (res (cleanUpList (cdr L)))
     )
       (if cur
     (cons cur res)
      res
     )
       );end let
     );end t
  );end cond
  );end

; getFirstBoxColumn (r col)
; Helper function for getFirstBoxPosition

(defun getFirstBoxColumn (r col)
  (cond ((null r) nil)
        (t (if (isBox (car r)) col
               (getFirstBoxColumn (cdr r) (+ col 1))))))

; getFirstBoxColumn (r col)
; Helper function for getFirstBoxPosition

(defun getFirstBoxPosition (s firstRow)
  (cond ((null s) nil)
        (t (let ((x (getFirstBoxColumn (car s) 0)))
             (if x (list x firstRow)
                   (getFirstBoxPosition (cdr s) (+ firstRow 1)))))))

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (not (getFirstBoxPosition s 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions for next-states;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-square-in-row (state column)
; Returns the square at the column in the row
(defun get-square-in-row (s c)
  (cond ((= c 0) (car s))
        (t (get-square-in-row (cdr s) (- c 1)))))

; get-square (state row column)
; Returns the integer in the state at the row and column
; specified.
(defun get-square (s r c)
  (cond ((null s) nil)
        ((or (< r 0) (< c 0)) nil)
        ((= r 0) (get-square-in-row (car s) c))
        (t (get-square (cdr s) (- r 1) c))))

; set-square-in-row (state column integer)
; sets the square at the column in the row
(defun set-square-in-row (s c v)
  (cond ((= c 0) (append (list v) (cdr s)))
        (t (append (list (car s)) (set-square-in-row (cdr s) (- c 1) v)))))

; set-square (state row column integer)
; Sets the integer in the state at the row and column
; specified.
(defun set-square (s r c v)
  (cond ((= r 0) (append (list (set-square-in-row (car s) c v)) (cdr s)))
        (t (append (list (car s)) (set-square (cdr s) (- r 1) c v)))))

; get-square-x-away (state direction x keeper-row keeper-column)
; Returns the square x away in a certain direction
(defun get-square-x-away (s d x keeper-row keeper-col)
  (cond ((= d 0) (get-square s (- keeper-row x) keeper-col))
        ((= d 1) (get-square s keeper-row (+ keeper-col x)))
        ((= d 2) (get-square s (+ keeper-row x) keeper-col))
        ((= d 3) (get-square s keeper-row (- keeper-col x)))))

; get-current-square (state direction keeper-row keeper-column)
; Returns the square underneath the keeper
(defun get-current-square (s d keeper-row keeper-col)
  (let ((square-at-keeper (get-square-x-away s d 0 keeper-row keeper-col)))
    (cond ((isKeeper square-at-keeper) blank)
          ((isKeeperStar square-at-keeper) star))))

; get-row-x-away (direction x keeper-row)
; Returns the row x away in a certain direction
(defun get-row-x-away (d x keeper-row)
  (cond ((= d 0) (- keeper-row x))
        ((= d 1) keeper-row)
        ((= d 2) (+ keeper-row x))
        ((= d 3) keeper-row)))

; get-col-x-away (direction x keeper-column)
; Returns the column x away in a certain direction
(defun get-col-x-away (d x keeper-col)
  (cond ((= d 0) keeper-col)
        ((= d 1) (+ keeper-col x))
        ((= d 2) keeper-col)
        ((= d 3) (- keeper-col x))))

; try-move (state direction)
; Returns the state after the move if the move is valid
; and NIL otherwise. The values for direction are as follows:
; 0 - up
; 1 - right
; 2 - down
; 3 - left
(defun try-move (s d)
  (let* ((keeper-row (cadr (getKeeperPosition s 0)))
         (keeper-col (car (getKeeperPosition s 0)))
         (square-current (get-current-square s d keeper-row keeper-col))
         (square-one-away (get-square-x-away s d 1 keeper-row keeper-col))
         (square-two-away (get-square-x-away s d 2 keeper-row keeper-col)))
    (cond ((null square-one-away) nil)   ; If there is nothing in the target square
          ((isWall square-one-away) nil) ; If there is a wall in the target square
          ((isBlank square-one-away)     ; If there is a blank in the target square
             (set-square (set-square s (get-row-x-away d 1 keeper-row) 
                                     (get-col-x-away d 1 keeper-col) keeper)
                         keeper-row keeper-col square-current))
          ((isStar square-one-away)      ; If there is a star in the target square
             (set-square (set-square s (get-row-x-away d 1 keeper-row) 
                                     (get-col-x-away d 1 keeper-col) keeperstar)
                         keeper-row keeper-col square-current))
          ((isBox square-one-away)       ; If there is a box in the target square
             (cond ((null square-two-away) nil)
                   ((isBlank square-two-away) ; If there is a blank after the target square
                    (set-square (set-square (set-square s (get-row-x-away d 2 keeper-row) 
                                                        (get-col-x-away d 2 keeper-col) box)
                                            (get-row-x-away d 1 keeper-row)
                                            (get-col-x-away d 1 keeper-col) keeper)
                                keeper-row keeper-col square-current))
                   ((isStar square-two-away) ; If there is a star after the target square
                    (set-square (set-square (set-square s (get-row-x-away d 2 keeper-row)
                                                        (get-col-x-away d 2 keeper-col) boxstar)
                                            (get-row-x-away d 1 keeper-row)
                                            (get-col-x-away d 1 keeper-col) keeper)
                                keeper-row keeper-col square-current))
                   ((isWall square-two-away) nil))) ; If there is a wall after the target square
          ((isBoxStar square-one-away)   ; If there is a boxstar in the target square
             (cond ((null square-two-away) nil)
                   ((isBlank square-two-away) ; If there is a blank after the target square
                    (set-square (set-square (set-square s (get-row-x-away d 2 keeper-row)
                                                        (get-col-x-away d 2 keeper-col) box)
                                            (get-row-x-away d 1 keeper-row)
                                            (get-col-x-away d 1 keeper-col) keeperstar)
                                keeper-row keeper-col square-current))
                   ((isStar square-two-away) ; If there is a star after the target square
                    (set-square (set-square (set-square s (get-row-x-away d 2 keeper-row)
                                                        (get-col-x-away d 2 keeper-col) boxstar)
                                            (get-row-x-away d 1 keeper-row)
                                            (get-col-x-away d 1 keeper-col) keeperstar)
                                keeper-row keeper-col square-current))
                   ((isWall square-two-away) nil)))))) ; If there is a wall after the target square

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
(defun next-states (s)
  (cleanUpList (list (try-move s 0) (try-move s 1) (try-move s 2) (try-move s 3))))

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s)
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions for h1 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-missplaced-boxes-in-row
; Returns the number of missplaced boxes in a row
(defun get-missplaced-boxes-in-row (row)
  (cond ((null (car row)) 0)
        ((isBox (car row)) (+ (get-missplaced-boxes-in-row (cdr row)) 1))
        (t (get-missplaced-boxes-in-row (cdr row)))))

; get-missplaced-boxes (state)
; Returns the number of missplaced boxes in a state
(defun get-missplaced-boxes (s)
  (cond ((null (car s)) 0)
        (t (+ (get-missplaced-boxes-in-row (car s)) (get-missplaced-boxes (cdr s))))))

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;
; This heuristic IS admissable as it will at no point overestimate the number of moves
; needed to complete the level. This is due to the fact that it is impossible to beat
; a level when there are two boxes misplaced with just one move.
;
(defun h1 (s)
  (get-missplaced-boxes s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions for h804275355 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; is-box-one-away (state row column)
; Returns true if there is a box one move away and
; false if there is no box one move away
(defun is-box-one-away (s r c)
  (let ((square-up    (or (get-square s (- r 1) c) -1))
        (square-right (or (get-square s r (+ c 1)) -1))
        (square-down  (or (get-square s (+ r 1) c) -1))
        (square-left  (or (get-square s r (- c 1)) -1)))
    (cond ((or (isBox square-up)
               (isBox square-right)
               (isBox square-down)
               (isBox square-left)) t)
          (t nil))))

; find-closest-box (state row column)
; Returns the closest box from a set of coordinates
(defun find-closest-box (s r c)
  (let ((cur-square (get-square s r c)))
    (cond ((null cur-square) nil)
          ((= cur-square 9) nil)
          ((isWall cur-square) nil)
          ((is-box-one-away s r c) 0)
          (t (let* ((new-square (set-square s r c 9))
                   (closest-up    (find-closest-box new-square (- r 1) c))
                   (closest-right (find-closest-box new-square r (+ c 1)))
                   (closest-down  (find-closest-box new-square (+ r 1) c))
                   (closest-left  (find-closest-box new-square r (- c 1)))
                   (distances (remove nil (list (if (not (null closest-up)) (+ closest-up 1))
                      (if (not (null closest-right)) (+ closest-right 1))
                      (if (not (null closest-down)) (+ closest-down 1))
                      (if (not (null closest-left)) (+ closest-left 1))))))
               (cond ((null distances) nil)
                     (t (apply 'min distances))))))))

; from-keeper-to-box (state)
; Returns the minimum number of moves needed to move the keeper
; from its current position to the closest box in the state
(defun from-keeper-to-box (s)
  (let ((keeper-row (cadr (getKeeperPosition s 0)))
       (keeper-col (car (getKeeperPosition s 0))))
    (find-closest-box s keeper-row keeper-col)))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;
; My heuristic function combines h1 with a function that computes
; how many moves it takes to move the keeper to the closest box.
; This accounts for boxes being close to their stars, but the keeper
; being very far away from the boxes.
;
(defun h804275355 (s)
  (let ((keeper-box-distance (from-keeper-to-box s)))
    (cond ((null keeper-box-distance) (get-missplaced-boxes s))
          (t (+ (get-missplaced-boxes s) keeper-box-distance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
     (1 0 3 0 0 1)
     (1 0 2 0 0 1)
     (1 1 0 1 1 1)
     (1 0 0 0 0 1)
     (1 0 0 0 4 1)
     (1 1 1 1 1 1)))

(setq p1finished '((1 1 1 1 1 1)
     (1 0 0 0 0 1)
     (1 0 4 0 0 1)
     (1 1 0 1 1 1)
     (1 0 0 0 0 1)
     (1 0 0 0 4 1)
     (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
     (1 0 0 0 0 0 1) 
     (1 0 0 0 0 0 1) 
     (1 0 0 2 1 4 1) 
     (1 3 0 0 1 0 1)
     (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
     (1 0 0 0 1 0 0 0 1)
     (1 0 0 0 2 0 3 4 1)
     (1 0 0 0 1 0 0 0 1)
     (1 0 0 0 1 0 0 0 1)
     (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
     (0 0 0 0 0 1 4)
     (0 0 0 0 0 0 0)
     (0 0 1 1 1 0 0)
     (0 0 1 0 0 0 0)
     (0 2 1 0 0 0 0)
     (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
     (1 1 0 0 1 1)
     (1 0 0 0 0 1)
     (1 4 2 2 4 1)
     (1 0 0 0 0 1)
     (1 1 3 1 1 1)
     (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
     (1 0 0 0 0 0 4 1)
     (1 0 0 0 2 2 3 1)
     (1 0 0 1 0 0 4 1)
     (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
     (0 0 1 1 1 1 0 0 0 3)
     (0 0 0 0 0 1 0 0 0 0)
     (0 0 0 0 0 1 0 0 1 0)
     (0 0 1 0 0 1 0 0 1 0)
     (0 2 1 0 0 0 0 0 1 0)
     (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
     (1 4 0 0 4 1)
     (1 0 2 2 0 1)
     (1 2 0 1 0 1)
     (1 3 0 0 4 1)
     (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
     (1 1 1 0 0 1 1 1 1) 
     (1 0 0 0 0 0 2 0 1) 
     (1 0 1 0 0 1 2 0 1) 
     (1 0 4 0 4 1 3 0 1) 
     (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
      (1 0 0 0 1 1 0)
      (1 3 2 0 0 1 1)
      (1 1 0 2 0 0 1)
      (0 1 1 0 2 0 1)
      (0 0 1 1 0 0 1)
      (0 0 0 1 1 4 1)
      (0 0 0 0 1 4 1)
      (0 0 0 0 1 4 1)
      (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
      (1 4 0 0 0 4 1)
      (1 0 2 2 1 0 1)
      (1 0 2 0 1 3 1)
      (1 1 2 0 1 0 1)
      (1 4 0 0 4 0 1)
      (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
      (1 1 1 1 1 0 0 0 1 1 1 1)
      (1 0 0 0 2 0 0 0 0 0 0 1)
      (1 3 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 2 1 1 1 0 0 0 1)
      (1 0 0 0 0 1 0 1 4 0 4 1)
      (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
      (1 4 0 0 0 0 0 2 0 1)
      (1 0 2 0 0 0 0 0 4 1)
      (1 0 3 0 0 0 0 0 2 1)
      (1 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 4 1)
      (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
      (0 2 1 4 0 0 0)
      (0 2 0 4 0 0 0)
      (3 2 1 1 1 0 0)
      (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
      (1 0 0 0 0 0 1)
      (1 0 0 2 2 0 1)
      (1 0 2 0 2 3 1)
      (1 4 4 1 1 1 1)
      (1 4 4 1 0 0 0)
      (1 1 1 1 0 0 0)
      ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
      (1 0 0 0 1 0 0 0)
      (1 2 1 0 1 1 1 1)
      (1 4 0 0 0 0 0 1)
      (1 0 0 5 0 5 0 1)
      (1 0 5 0 1 0 1 1)
      (1 1 1 0 3 0 1 0)
      (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
      (1 3 0 0 1 0 0 0 4 1)
      (1 0 2 0 2 0 0 4 4 1)
      (1 0 2 2 2 1 1 4 4 1)
      (1 0 0 0 0 1 1 4 4 1)
      (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
      (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
      (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
      (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
      (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
      (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
      (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
      ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
      (0 0 0 1 0 0 0 0 1 0 0 0)
      (0 0 0 1 0 0 0 0 1 0 0 0)
      (1 1 1 1 0 0 0 0 1 1 1 1)
      (0 0 0 0 1 0 0 1 0 0 0 0)
      (0 0 0 0 0 0 3 0 0 0 2 0)
      (0 0 0 0 1 0 0 1 0 0 0 4)
      (1 1 1 1 0 0 0 0 1 1 1 1)
      (0 0 0 1 0 0 0 0 1 0 0 0)
      (0 0 0 1 0 0 0 0 1 0 0 0)
      (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
      (1 1 1 1 0 0 1 1 0)
      (1 0 0 0 2 0 0 1 0)
      (1 0 0 5 5 5 0 1 0)
      (1 0 0 4 0 4 0 1 1)
      (1 1 0 5 0 5 0 0 1)
      (0 1 1 5 5 5 0 0 1)
      (0 0 1 0 2 0 1 1 1)
      (0 0 1 0 3 0 1 0 0)
      (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
      (1 1 1 0 0 1 1 1 1 0)
      (1 0 0 2 0 0 0 1 1 0)
      (1 3 2 0 2 0 0 0 1 0)
      (1 1 0 2 0 2 0 0 1 0)
      (0 1 1 0 2 0 2 0 1 0)
      (0 0 1 1 0 2 0 0 1 0)
      (0 0 0 1 1 1 1 0 1 0)
      (0 0 0 0 1 4 1 0 0 1)
      (0 0 0 0 1 4 4 4 0 1)
      (0 0 0 0 1 0 1 4 0 1)
      (0 0 0 0 1 4 4 4 0 1)
      (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
      (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
      (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
      (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
      (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
      (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
      (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
      (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
      (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
   (k2 (getKeeperPosition s2 0))
   (deltaX (- (car k2) (car k1)))
   (deltaY (- (cadr k2) (cadr k1)))
   )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
    (t (if (> deltaX 0) 'RIGHT 'LEFT))
    );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
  ((= 1 (length m)) (list 'END))
  (t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
  );end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
  ((= s wall) (format t "#"))
  ((= s box) (format t "$"))
  ((= s keeper) (format t "@"))
  ((= s star) (format t "."))
  ((= s boxstar) (format t "*"))
  ((= s keeperstar) (format t "+"))
  (t (format t "|"))
  );end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
