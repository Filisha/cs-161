; CS 161 - HW 4
; N-QUEENS Solver

; reload
; Reloads the file in lisp
(defun reload ()
  (load "hw4.lsp")) 

; has-duplicates (list)
; Returns t   if list has duplicate elements
;         nil otherwise
(defun has-duplicates (list)
  (cond ((equal (remove-duplicates list) list) nil)
        (t t)))

; get-row-column-sums (state column)
; Returns a list containing the sums of row and column for each queen
(defun get-row-column-sums (s c)
  (cond ((null (car s)) nil)
        ((= (car s) 0) nil)
        (t (append (list (+ (car s) c)) (get-row-column-sums (cdr s) (+ c 1))))))

; get-row-column-differences (state column)
; Returns a list containing the differences of row and column for each queen
(defun get-row-column-differences (s c)
  (cond ((null (car s)) nil)
        ((= (car s) 0) nil)
        (t (append (list (- (car s) c)) (get-row-column-differences (cdr s) (+ c 1))))))

; check-top-right-diagonal (state)
; Returns t   if state is valid along the diagonal from the bottom left to the top right
;         nil otherwise
(defun check-top-right-diagonal (s)
  (cond ((has-duplicates (get-row-column-sums s 1)) nil)
        (t t)))

; check-top-left-diagonal (state)
; Returns t   if state is valid along the diagonal from the bottom right to the top left
;         nil otherwise
(defun check-top-left-diagonal (s)
  (cond ((has-duplicates (get-row-column-differences s 1)) nil)
        (t t)))

; check-horizontal (state n)
; Returns t   if state is horizontally valid and complete
;         nil otherwise
(defun check-horizontal (s n)
  (and (not (has-duplicates s))
       (not (position '0 s))))

; check-state (state)
; Returns t   if all queens are in valid positions
;         nil otherwise
(defun check-state (s)
  (and (check-top-left-diagonal s) 
       (check-top-right-diagonal s)
       (not (has-duplicates (remove '0 s)))))

; goal-state (state n)
; Returns t   if all queens in s are in valid positions
;         nil otherwise
(defun goal-state (s n)
  (cond ((and (check-state s) (check-horizontal s n)) t)
        (t nil)))

; set-n (state index new-value count)
; Return a new list with value new-value at index in state
(defun set-n (s n v count)
  (cond ((= count n) (append (list v) (cdr s)))
        (t (append (list (car s)) (set-n (cdr s) n v (+ count 1))))))

; create-next-states-list (state position current max)
; Helper function for next-states
(defun create-next-states-list (s pos cur max)
  (cond ((> cur max) nil)
        (t (let* ((new-state (set-n s pos cur 0))
                  (valid (check-state new-state)))
             (cond ((not (null valid)) (append (list new-state) (create-next-states-list s pos (+ cur 1) max)))
                   (t (append (create-next-states-list s pos (+ cur 1) max))))))))

; next-states (state)
; Returns a list of valid next states from state
(defun next-states (s)
  (create-next-states-list s (position '0 s) 1 (length s)))

; queens-dfs-helper (next n)
; Helper function for queens-dfs
(defun queens-dfs-helper (next n)
  (cond ((null next) nil)
        (t (let ((return (queens-dfs (car next) n)))
             (cond ((not (null return)) return)
                   (t (queens-dfs-helper (cdr next) n)))))))

; queens-dfs (state n)
; Using depth-first search, returns the first valid n-queens solution
(defun queens-dfs (s n)
  (cond ((goal-state s n) s)
        ((null s) nil)
        (t (queens-dfs-helper (next-states s) n))))

; create-zeros-list (n count)
; Returns a list containing n zeros
(defun create-zeros-list (n count)
  (cond ((= n count) nil)
        (t (append '(0) (create-zeros-list n (+ count 1))))))

; queens (n)
; Primary function for solving N-QUEENS, calls queens-dfs that executes DFS
(defun queens (n)
  (queens-dfs (create-zeros-list n 0) n))
