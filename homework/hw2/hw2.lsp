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
  )
