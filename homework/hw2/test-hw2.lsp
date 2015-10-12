(load "hw2.lsp")

(cond
  ((and (equal (DFS '((w x) (y z))) '(w x y z))
        (equal (DFS '((a (b)) c (d))) '(a b c d)))
        (equal (DFS '(a (b c) (d) (e (f g)))) '(a b c d e f g))
   (print "All test cases passed for DFS."))
  (t (print "Some test cases failed for DFS!")))