;;;; 1. TREE-CONTAINS
;;; params:  N    - number
;;;          TREE - ordered tree
;;; returns: T    - TREE contains N
;;;          NIL  - TREE does not contain N
(defun TREE-CONTAINS (N TREE)
  (cond ((numberp TREE) 
         (cond ((= N TREE) t)
               ((< N TREE) NIL)
               ((> N TREE) NIL)))
        ((= 3 (length TREE)) 
         (cond ((null TREE) NIL)
               ((= N (cadr TREE)) t)
               ((< N (cadr TREE)) (TREE-CONTAINS N (car TREE)))
               ((> N (cadr TREE)) (TREE-CONTAINS N (caddr TREE)))))))

;;;; 2. TREE-MAX
;;; params:  TREE - ordered tree
;;; returns: largest number in TREE
(defun TREE-MAX (TREE)
  (cond ((numberp TREE) TREE)
        ((= 3 (length TREE)) (TREE-MAX (caddr TREE)))))

;;;; 3. TREE-ORDER
;;; params:  TREE - ordered tree
;;; returns: in-ordered list of numbers in TREE

;;;; 4. SUB-LIST
;;; params:  L     - list expression
;;;          START - start position
;;;          LEN   - length of sub-list
;;; returns: sub-list of L of length LEN starting at START

;;;; 5. SPLIT-LIST
;;; params:  L     - list expression
;;; returns: list of two lists L1 and L2 where L1 and L2 combined make L
;;;          and the length of L1 minus the length of L2 is 0 or 1

;;;; 6. BTREE-HEIGHT
;;; params:  L     - list expression
;;;          START - start position
;;;          LEN   - length of sub-list
;;; returns: sub-list of L of length LEN starting at START
