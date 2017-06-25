(load "base.scm")
(load "pink.scm")
(load "mk.scm")
(load "test-check.scm")

(test "mk-1"
  (evalms (list (mk '(let p ((call/fresh (lambda _ q ((== q) 5))) empty-state) (car p)))) `((,pink-eval-exp1 (var 0)) nil-env))
  '((((var . 0) . 5)) . 1)
)

(test "mk-2"
  (evalms (list (mk '(let p ((call/fresh (lambda _ q ((== q) 5))) empty-state) (cdr p)))) `((,pink-eval-exp1 (var 0)) nil-env))
  '()
)


(define a-and-b
'((conj
   (call/fresh (lambda _ a ((== a) 7))))
   (call/fresh
    (lambda _ b
      ((disj
       ((== b) 5))
       ((== b) 6)))))
)

(test "mk-3"
  (evalms (list (mk `(let p (,a-and-b empty-state) (car p)))) `((,pink-eval-exp1 (var 0)) nil-env))
  '((((var . 1) . 5) ((var . 0) . 7)) . 2)
)

(test "mk-3"
  (evalms (list (mk `(let p (,a-and-b empty-state) (car (cdr p))))) `((,pink-eval-exp1 (var 0)) nil-env))
  '((((var . 1) . 6) ((var . 0) . 7)) . 2)
)

(test "mk-4"
  (evalms (list (mk `(let p (,a-and-b empty-state) (cdr (cdr p))))) `((,pink-eval-exp1 (var 0)) nil-env))
  '()
)

(test "mk-compiled"
  (let ((p (evalms (list (mk `(clambda _ _ (,a-and-b empty-state)))) `((,pink-eval-exp1 (var 0)) nil-env)))) (evalms (list p) '((var 0) 0)))
  (let ((p (evalms (list (mk `(lambda  _ _ (,a-and-b empty-state)))) `((,pink-eval-exp1 (var 0)) nil-env)))) (evalms (list p) '((var 0) 0)))
)
