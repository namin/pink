(load "base.scm")
(load "pink.scm")
(load "test-check.scm")

(test "pink-code-1"
  (evalms '() (trans `(code? 0 1) '()))
  0
)

(test "pink-eval-code-1"
  (evalms '((code? 0 1)) `((,pink-eval-exp1 (var 0)) nil-env))
  0
)

(test "pink-code-lift-2"
  (evalms '() (trans `(code? 0 (lift 2)) '()))
  1
)

(test "pink-eval-code-lift-21"
  (evalms '((code? 0 (lift 2))) `((,pink-eval-exp1 (var 0)) nil-env))
  1
)

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

(test "pink-eval-evalc-fac"
  (reifyc (lambda () (evalms (list pink-fac pink-evalc-src) `((((,pink-eval-exp2 (var 1)) nil-env) (var 0)) nil-env))))
  base-fac-anf
)

(test "pink-eval-eval-evalc-fac"
  (reifyc (lambda () (evalms (list pink-fac pink-evalc-src pink-eval-src) `((((((,pink-eval-exp3 (var 2)) nil-env) (var 1)) nil-env) (var 0)) nil-env))))
  base-fac-anf
)

;; slow but true
;; (test "pink-eval-eval-eval-evalc-fac"
;;   (reifyc (lambda () (evalms (list pink-fac pink-evalc-src pink-eval-src) `((((((((,pink-eval-exp3 (var 2)) nil-env) (var 2)) nil-env) (var 1)) nil-env) (var 0)) nil-env))))
;;   base-fac-anf
;; )

(test "pink-evalc-fac-4"
  (let ((c (reifyc (lambda () (evalms (list pink-fac) `((,pink-evalc-exp1 (var 0)) nil-env))))))
    (run (lambda () (evalms '() `(,c 4)))))
  24
)

(test "pink-self-compilation"
  (let ((c (reifyc (lambda () (evalms (list pink-eval-src) `((,pink-evalc-exp1 (var 0)) nil-env))))))
    (run (lambda () (let ((v (evalms '() c)))
                 (evalms (list pink-fac v) '((((var 1) (var 0)) nil-env) 4))))))
  24
)

(test "pink-trace-fac"
  (evalms (list
     '((delta-eval (lambda _ tie (lambda _ eval (lambda ev l (lambda _ exp (lambda _ env
     (if (if (symbol? exp) (eq? 'n exp) 0) (log ((car l) 0) (((eval l) exp) env))
     ((((tie ev) l) exp) env)))))))
     (lambda f n (if n (* n (f (- n 1))) 1))) 4))
    `((,pink-eval-exp1 (var 0)) nil-env))
  24
)

(test "pink-trace-fac-2"
  (evalms (list
     '(delta-eval (lambda _ tie (lambda _ eval (lambda ev l (lambda _ exp (lambda _ env
     (if (if (symbol? exp) (eq? 'n exp) 0) (log ((car l) 0) (((eval l) exp) env))
     ((((tie ev) l) exp) env)))))))
     (lambda f n (if n (* n (f (- n 1))) 1))))
    `(((,pink-eval-exp1 (var 0)) nil-env) 4))
  24
)

(test "pink-trace-fac-clambda"
  (evalms (list
     '(delta-eval (lambda _ tie (lambda _ eval (lambda ev l (lambda _ exp (lambda _ env
     (if (if (symbol? exp) (eq? 'n exp) 0) (log ((car l) 0) (((eval l) exp) env))
     ((((tie ev) l) exp) env)))))))
     (clambda f n (if n (* n (f (- n 1))) 1))))
    `(((,pink-eval-exp1 (var 0)) nil-env) 4))
  24
)

(test "pink-fac-clambda-code"
  (let ((c (evalms (list
     '(clambda f n (if n (* n (f (- n 1))) 1)))
    `((,pink-eval-exp1 (var 0)) nil-env))))
    (caddr c))
  '(let (if [var 1]
      [let (- (var 1) 1)
        (let ([var 0] [var 2]) (let (* [var 1] [var 3]) (var 4)))]
      1)
   (var 2))
)

(test "pink-trace-fac-clambda-code"
  (let ((c (evalms (list
     '(delta-eval (lambda _ tie (lambda _ eval (lambda ev l (lambda _ exp (lambda _ env
     (if (if (symbol? exp) (eq? 'n exp) 0) (log ((car l) 0) (((eval l) exp) env))
     ((((tie ev) l) exp) env)))))))
     (clambda f n (if n (* n (f (- n 1))) 1))))
    `((,pink-eval-exp1 (var 0)) nil-env))))
    (caddr c))
  '(let (log 0 [var 1])
   (let (if
        [var 2]
        [let (log 0 (var 1))
          (let (log 0 [var 1])
            (let (- [var 4] 1)
              (let ([var 0] [var 5]) (let (* [var 3] [var 6]) (var 7)))))]
        1)
    (var 3)))
)

(test "pink-unlift-oo"
(evalms (list `(clambda _ _
                (let send (unlift (lambda _ o (unlift (lambda _ msg (o msg)))))
                (let recv (unlift (lambda _ t t))
               ((send (recv (unlift (lambda _ msg (if (eq? msg 'hi) (lift 'hello) (lift 'error)))))) (unlift 'hi))))))
        `((,pink-eval-exp1 (var 0)) nil-env))
'(clo () hello)
)
