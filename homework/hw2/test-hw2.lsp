(load "hw2.lsp")

(cond
  ((and (equal (DFS '((w x) (y z))) '(w x y z))
        (equal (DFS '((a (b)) c (d))) '(a b c d)))
        (equal (DFS '(a (b c) (d) (e (f g)))) '(a b c d e f g))
   (print "All test cases passed for DFS."))
  (t (print "Some test cases failed for DFS!")))

(cond
  ((and (equal (DFID '((A (B)) C (D)) '(C A C D A B C D))))
   (print "All test cases passed for DFID."))
  (t (print "Some test cases failed for DFID!")))

