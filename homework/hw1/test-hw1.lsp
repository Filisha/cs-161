(load "hw1.lsp")

(cond
  ((and (TREE-CONTAINS 3 '((1 2 3) 7 8))
        (not (TREE-CONTAINS 4 '((1 2 3) 7 8))))
   (print "All test cases passed for TREE-CONTAINS."))
  (t (print "Some test cases failed for TREE-CONTAINS!")))

(cond
  ((and (equal (TREE-MAX '((1 2 3) 7 8)) 8)
        (equal (TREE-MAX '((1 2 3) 7 (8 9 10))) 10))
   (print "All test cases passed for TREE-MAX."))
  (t (print "Some test cases failed for TREE-MAX!")))

(cond
  ((and (equal (TREE-ORDER 3) '(3))
        (equal (TREE-ORDER '((1 2 3) 7 8)) '(1 2 3 7 8)))
   (print "All test cases passed for TREE-ORDER."))
  (t (print "Some test cases failed for TREE-ORDER!")))

(cond
  ((and (equal (SUB-LIST '(a b c d) 0 3) '(a b c))
        (equal (SUB-LIST '(a b c d) 3 1) '(d))
        (equal (SUB-LIST '(a b c d) 2 0) '()))
   (print "All test cases passed for SUB-LIST."))
  (t (print "Some test cases failed for SUB-LIST!")))

(cond
  ((and (equal (SPLIT-LIST '(a b c d)) '((a b) (c d)))
        (equal (SPLIT-LIST '(a b c d e)) '((a b) (c d e)))
        (equal (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f))))
   (print "All test cases passed for SPLIT-LIST."))
  (t (print "Some test cases failed for SPLIT-LIST!")))

(cond
  ((and (equal (BTREE-HEIGHT 1) 0)
        (equal (BTREE-HEIGHT '(1 2)) 1)
        (equal (BTREE-HEIGHT '(1 (2 3))) 2)
        (equal (BTREE-HEIGHT '((1 2) (3 4))) 2)
        (equal (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3)
        (equal (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3))
   (print "All test cases passed for BTREE-HEIGHT."))
  (t (print "Some test cases failed for BTREE-HEIGHT!")))

(cond
  ((and (equal (LIST2BTREE '(1)) 1)
        (equal (LIST2BTREE '(1 2)) '(1 2))
        (equal (LIST2BTREE '(1 2 3)) '(1 (2 3)))
        (equal (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4)))
        (equal (LIST2BTREE '(1 2 3 4 5 6 7)) '((1 (2 3)) ((4 5) (6 7))))
        (equal (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8)))))
   (print "All test cases passed for LIST2BTREE."))
  (t (print "Some test cases failed for LIST2BTREE!")))

(cond
  ((and (equal (BTREE2LIST 1) '(1))
        (equal (BTREE2LIST '(1 2)) '(1 2))
        (equal (BTREE2LIST '(1 (2 3))) '(1 2 3))
        (equal (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4))
        (equal (BTREE2LIST '((1 (2 3)) ((4 5) (6 7)))) '(1 2 3 4 5 6 7))
        (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8)))
   (print "All test cases passed for BTREE2LIST."))
  (t (print "Some test cases failed for BTREE2LIST!")))
