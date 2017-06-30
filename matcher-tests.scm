(load "base.scm")
(load "pink.scm")
(load "matcher.scm")
(load "test-check.scm")

(test "matcher-1"
  (evalms (list `(let maybe-lift (lambda _ e e) ,matcher-src)
                `(_ * a _ * done) `(b a done))
          `((((,pink-eval-exp3 (var 0)) nil-env) (var 1)) (var 2)))
  'yes
)

(test "matcher-2"
  (evalms (list `(let maybe-lift (lambda _ e e) ,matcher-src)
                `(_ * a _ * done) `(b b done))
          `((((,pink-eval-exp3 (var 0)) nil-env) (var 1)) (var 2)))
  'no
)

(test "matcher-c-1"
  (let ((c (reifyc (lambda () (evalms (list `(let maybe-lift (lambda _ e (lift e)) ,matcher-src)
                                       `(_ * a _ * done))
                                 `(((,pink-eval-exp2 (var 0)) nil-env) (var 1)))))))
    (run (lambda () (let ((v (evalms '() c)))
                 (evalms (list `(b a done) v) `((var 1) (var 0)))))))
  'yes
)

(test "matcher-c-2"
  (let ((c (reifyc (lambda () (evalms (list `(let maybe-lift (lambda _ e (lift e)) ,matcher-src)
                                       `(_ * a _ * done))
                                 `(((,pink-eval-exp2 (var 0)) nil-env) (var 1)))))))
    (run (lambda () (let ((v (evalms '() c)))
                 (evalms (list `(b b done) v) `((var 1) (var 0)))))))
  'no
)

(test "matcher-trace-1"
  (evalms (list
     `(delta-eval (lambda _ tie (lambda _ eval (lambda ev l (lambda _ exp (lambda _ env
     (if (symbol? exp) (let _ (log exp) (log (((eval l) exp) env)))
     ((((tie ev) l) exp) env)))))))
     (let maybe-lift (lambda _ e e) ,matcher-src))
     `(_ * a _ * done) `(b a done))
     `((((,pink-eval-exp3 (var 0)) nil-env) (var 1)) (var 2)))
  'yes
)

(test "matcher-trace-2"
  (evalms (list
     `(delta-eval (lambda _ tie (lambda _ eval (lambda ev l (lambda _ exp (lambda _ env
     (if (symbol? exp) (let _ (log exp) (log (((eval l) exp) env)))
     ((((tie ev) l) exp) env)))))))
     (let maybe-lift (lambda _ e e) ,matcher-src))
     `(_ * a _ * done) `(b b done))
     `((((,pink-eval-exp3 (var 0)) nil-env) (var 1)) (var 2)))
  'no
)

(define tracing-matcher-transformer
  (lambda (r)
  (let ((c (reifyc (lambda () (evalms (list
     `(delta-eval (lambda _ tie (lambda _ eval (lambda ev l (lambda _ exp (lambda _ env
     (if (symbol? exp) (let _ (log (lift exp)) (let r (((eval l) exp) env) (if (code? 0 r) (log r) (let _ (log (lift-ref exp r)) r))))
     ((((tie ev) l) exp) env)))))))
     (let maybe-lift (lambda _ e (lift e)) ,matcher-src))
     r)
     `(((,pink-eval-exp2 (var 0)) nil-env) (var 1)))))))
    (run (lambda () (let ((v (evalms '() c))) v))))))


(test "matcher-trace-c-1"
  (evalms (list `(b a done) (tracing-matcher-transformer '(_ * a _ * done))) `((var 1) (var 0)))
  'yes
)

(test "matcher-trace-c-2"
  (evalms (list `(b b done) (tracing-matcher-transformer '(_ * a _ * done))) `((var 1) (var 0)))
  'no
)
