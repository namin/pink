(load "base.scm")
(load "test-check.scm")

(test "fac-4"
  (run (lambda () (evalms '() `((lambda (if (var 1) (* (var 1) ((var 0) (- (var 1) 1))) 1)) 4))))
  24)

(test "number?-1"
  (run (lambda () (evalms '() '(number? 1))))
  1)

(test "reifyc-1"
  (reifyc (lambda () (evalms '() `(if (lift 0) (+ (lift 1) (lift 2)) (lift 0)))))
  '(let (if 0 (let (+ 1 2) (var 0)) 0) (var 0)))

(test "reifyc-fac"
  (reifyc (lambda () (evalms '() `(lift (lambda (if (var 1) (* (var 1) ((var 0) (- (var 1) (lift 1)))) (lift 1)))))))
  '(let (lambda
   (let (if (var 1)
            (let (- (var 1) 1)
            (let ((var 0) (var 2)) (let (* (var 1) (var 3)) (var 4))))
            1)
     (var 2)))
     (var 0)))
