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
(defun TREE-ORDER (TREE)
  (cond ((numberp TREE) (list TREE))
        ((= 3 (length TREE)) (append (TREE-ORDER (car TREE)) (list (cadr TREE)) 
          (TREE-ORDER (caddr TREE))))))

;;;; 4. SUB-LIST
;;; params:  L     - list expression
;;;          START - start position
;;;          LEN   - length of sub-list
;;; returns: sub-list of L of length LEN starting at START
(defun SUB-LIST (L START LEN)
  (cond ((= LEN 0) NIL)
        ((= START 0) (append (list (car L)) (SUB-LIST (cdr L) 0 (- LEN 1))))
        (t (SUB-LIST (cdr L) (- START 1) LEN))))

;;;; 5. SPLIT-LIST
;;; params:  L     - list expression
;;; returns: list of two lists L1 and L2 where L1 and L2 combined make L and 
;;; the length of L2 minus the length of L1 is 0 or 1
(defun SPLIT-LIST (L)
  (cond ((oddp (length L)) (list (SUB-LIST L 0 (/ (- (length L) 1) 2))
                                 (SUB-LIST L (/ (- (length L) 1) 2) (- (length L) (/ (- (length L) 1) 2)))))
        (t (list (SUB-LIST L 0 (/ (length L) 2))
                 (SUB-LIST L (/ (length L) 2) (- (length L) (/ (length L) 2)))))))

;;;; 6. BTREE-HEIGHT
;;; params:  TREE - binary tree
;;; returns: the height of TREE
(defun BTREE-HEIGHT (TREE)
  (cond ((numberp TREE) 0)
        ((= 2 (length TREE))
         (let ((MAX-LEFT (BTREE-HEIGHT (car TREE)))
               (MAX-RIGHT (BTREE-HEIGHT (cadr TREE))))
           (cond ((> MAX-RIGHT MAX-LEFT) (+ 1 MAX-RIGHT))
                 (t (+ 1 MAX-LEFT)))))))

;;;; 7. LIST2BTREE
;;; params:  LEAVES - a nonempty list expression
;;; returns: a binary tree with elements from LEAVES and where
(defun LIST2BTREE (LEAVES)
  (cond ((= 1 (length LEAVES)) (car LEAVES))
        (t (let ((SPLIT (SPLIT-LIST LEAVES))) 
             (list (LIST2BTREE (car SPLIT)) 
                   (LIST2BTREE (cadr SPLIT)))))))

;;;; 8. BTREE2LIST
;;; params: TREE - binary tree
;;; returns: a list expression so that BTREE2LIST is the inverse of LIST2BTREE
(defun BTREE2LIST (TREE)
  (cond ((numberp TREE) (list TREE))
        (t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))))
