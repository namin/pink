(load "base.scm")
(load "pink.scm")
(load "test-check.scm")

(test "pink-fac-4"
  (evalms '() (trans `(,pink-fac 4) '()))
  24
)

(test "pink-eval-fac-4"
  (evalms (list pink-fac) `(((,pink-eval-exp1 (var 0)) nil-env) 4))
  24
)

(test "pink-evalc-fac"
  (reifyc (lambda () (evalms (list pink-fac) `((,pink-evalc-exp1 (var 0)) nil-env))))
  base-fac-anf
)

(test "pink-evalc-fac-4"
  (let ((c (reifyc (lambda () (evalms (list pink-fac) `((,pink-evalc-exp1 (var 0)) nil-env))))))
    (run (lambda () (evalms '() `(,c 4)))))
  24
)
