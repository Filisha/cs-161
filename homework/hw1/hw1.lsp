;;;; 1. TREE-CONTAINS
;;; params:      N    - number
;;;              TREE - ordered tree
;;; returns:     T    - TREE contains N
;;;              NIL  - TREE does not contain N
;;; description: TREE-CONTAINS first checks if the node is a tree or a 
;;;              number. If it is a number, if checks if its value is
;;;              equal to N. Ff it is return true, otherwise return false.
;;;              If it is not a number, the function checks if the node 
;;;              value is equal to N. If it is return true, otherwise 
;;;              proceed down the tree.
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
;;; params:      TREE - ordered tree
;;; returns:     largest number in TREE
;;; description: TREE-MAX simply checks if the current node of TREE is
;;;              a number. If it is, this is the maximum and the value 
;;;              is then returned. If the node is itself a tree, then
;;;              return TREE-MAX of the right-hand branch.
(defun TREE-MAX (TREE)
  (cond ((numberp TREE) TREE)
        ((= 3 (length TREE)) (TREE-MAX (caddr TREE)))))

;;;; 3. TREE-ORDER
;;; params:      TREE - ordered tree
;;; returns:     in-ordered list of numbers in TREE
;;; description: TREE-ORDER first checks if the node of the TREE being
;;;              checked is a number. If it is return the list 
;;;              containing just that number. If the node is a tree, 
;;;              combine TREE-ORDER of the left branch, the center 
;;;              node, and TREE-ORDER of the right branch and return
;;;              this combined list.
(defun TREE-ORDER (TREE)
  (cond ((numberp TREE) (list TREE))
        ((= 3 (length TREE)) (append (TREE-ORDER (car TREE)) (list (cadr TREE)) 
          (TREE-ORDER (caddr TREE))))))

;;;; 4. SUB-LIST
;;; params:      L     - list expression
;;;              START - start position
;;;              LEN   - length of sub-list
;;; returns:     sub-list of L of length LEN starting at START
;;; description: SUB-LIST first checks if the length parameter is 0, 
;;;              if it is return NIL. It then checks if the start 
;;;              parameter is 0. If it is, return the combined list 
;;;              consisting of the first element of L and the SUB-LIST
;;;              of the rest of L with a start of 0 and a length of 
;;;              LEN-1. If start is not 0 and length is not 0, return
;;;              the SUB-LIST consisting of the rest of L, a start of
;;;              START-1 and a length of LEN.
(defun SUB-LIST (L START LEN)
  (cond ((= LEN 0) NIL)
        ((= START 0) (append (list (car L)) (SUB-LIST (cdr L) 0 (- LEN 1))))
        (t (SUB-LIST (cdr L) (- START 1) LEN))))

;;;; 5. SPLIT-LIST
;;; params:      L     - list expression
;;; returns:     list of two lists L1 and L2 where L1 and L2 combined make L and 
;;;              the length of L2 minus the length of L1 is 0 or 1
;;; description: SPLIT-LIST works with two conditions: if the length
;;;              of the list L is odd, then create two lists by 
;;;              calling SUB-LIST with the second list containing one 
;;;              more element than the first. If L's length is even, 
;;;              then evenly create two SUB-LISTS from L.
(defun SPLIT-LIST (L)
  (cond ((oddp (length L)) (list (SUB-LIST L 0 (/ (- (length L) 1) 2))
                                 (SUB-LIST L (/ (- (length L) 1) 2) (- (length L) (/ (- (length L) 1) 2)))))
        (t (list (SUB-LIST L 0 (/ (length L) 2))
                 (SUB-LIST L (/ (length L) 2) (- (length L) (/ (length L) 2)))))))

;;;; 6. BTREE-HEIGHT
;;; params:      TREE - binary tree
;;; returns:     the height of TREE
;;; description: BTREE-HEIGHT first checks if the TREE parameter is a
;;;              number. If it is return 0, as the TREE has no height.  
;;;              If the parameter is a binary tree, set MAX-LEFT to be 
;;;              the BTREE-HEIGHT of the left branch, and MAX-RIGHT to 
;;;              be the BTREE-HEIGHT of the right branch. Then return 
;;;              the larger height + 1 to account for the current node.
(defun BTREE-HEIGHT (TREE)
  (cond ((numberp TREE) 0)
        ((= 2 (length TREE))
         (let ((MAX-LEFT (BTREE-HEIGHT (car TREE)))
               (MAX-RIGHT (BTREE-HEIGHT (cadr TREE))))
           (cond ((> MAX-RIGHT MAX-LEFT) (+ 1 MAX-RIGHT))
                 (t (+ 1 MAX-LEFT)))))))

;;;; 7. LIST2BTREE
;;; params:      LEAVES - a nonempty list expression
;;; returns:     a binary tree with elements from LEAVES and where
;;; description: LIST2BTREE first checks if the list of LEAVES contains
;;;              only one leaf. If true, it returns this leaf. If 
;;;              LEAVES contains more than one leaf, set SPLIT to be 
;;;              the SPLIT-LIST of LEAVES. Then return a list 
;;;              consisting of LIST2BTREE of the first value of SPLIT
;;;              and LIST2BTREE of the second value of SPLIT.
(defun LIST2BTREE (LEAVES)
  (cond ((= 1 (length LEAVES)) (car LEAVES))
        (t (let ((SPLIT (SPLIT-LIST LEAVES))) 
             (list (LIST2BTREE (car SPLIT)) 
                   (LIST2BTREE (cadr SPLIT)))))))

;;;; 8. BTREE2LIST
;;; params:      TREE - binary tree
;;; returns:     a list expression so that BTREE2LIST is the inverse of LIST2BTREE
;;; description: BTREE2LIST first checks if TREE is a number. If it
;;;              is, return a list consisting of that number. If TREE 
;;;              is a binary tree, create a list consisting of 
;;;              BTREE2LIST of the first element of TREE and 
;;;              BTREE2LIST of the second element of LIST.
(defun BTREE2LIST (TREE)
  (cond ((numberp TREE) (list TREE))
        (t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))))
