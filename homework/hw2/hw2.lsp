;;;; 1. DFS
;;; params:      TREE - list representation of a tree
;;; returns:     a list of terminal nodes in order visited
;;; description: 
(defun DFS (TREE)
  (cond ((null TREE) nil)
        ((atom TREE) (list TREE))
        (t (append (DFS (car TREE)) (DFS (cdr TREE))))))

;;;; 2. DFID
;;; params:      TREE - list representation of a tree
;;;              LIMIT - depth limit of search
;;; returns:     a list of terminal nodes in order visited
;;; description: 
(defun DFID (TREE LIMIT)
  (cond ((equal LIMIT 0) (DFS-LIMITED TREE 0 LIMIT))
        (t (append (DFID TREE (- LIMIT 1)) (DFS-LIMITED TREE 0 LIMIT)))))

(defun DFS-LIMITED (TREE LEVEL LIMIT)
  (cond ((null TREE) nil)
        ((atom TREE) (list TREE))
        ((equal LEVEL LIMIT) nil)
        (t (append (DFS-LIMITED (car TREE) (+ LEVEL 1) LIMIT) (DFS-LIMITED (cdr TREE) (+ LEVEL 1) LIMIT)))))