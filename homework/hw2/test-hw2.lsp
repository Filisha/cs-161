(load "hw2.lsp")

(cond
  ((and (equal (DFS '((w x) (y z))) '(w x y z))
        (equal (DFS '((a (b)) c (d))) '(a b c d)))
        (equal (DFS '(a (b c) (d) (e (f g)))) '(a b c d e f g))
   (print "All test cases passed for DFS."))
  (t (print "Some test cases failed for DFS!")))

(cond
  ((equal (DFID '((A (B)) C (D)) 3) '(C A C D A B C D))
   (print "All test cases passed for DFID."))
  (t (print "Some test cases failed for DFID!")))

(cond
  ((equal (MC-DFS '(3 3 T) NIL) '((3 3 T) (1 1 NIL) 
                                 (3 2 T) (0 3 NIL) 
                                 (3 1 T) (2 2 NIL) 
                                 (2 2 T) (3 1 NIL) 
                                 (0 3 T) (3 2 NIL) 
                                 (1 1 T) (3 3 NIL)))
   (print "All test cases passed for MC-DFS."))
  (t (print "Some test cases failed for MC-DFS!")))

;;;  m c        m c 
;;; ================
;;;  0 0 |   x| 3 3
;;;  1 1 |x   | 2 2
;;;  0 1 |   x| 3 2
;;;  0 3 |x   | 3 0
;;;  0 2 |   x| 3 1
;;;  2 2 |x   | 1 1
;;;  1 1 |   x| 2 2
;;;  3 1 |x   | 0 2
;;;  3 0 |   x| 0 3
;;;  3 2 |x   | 0 1
;;;  2 2 |   x| 1 1
;;;  3 3 |x   | 0 0
;;; ================
