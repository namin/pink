(load "base.scm")
(load "test-check.scm")

(test "fac-4"
  (evalms '() `((lambda (if (var 1) (* (var 1) ((var 0) (- (var 1) 1))) 1)) 4))
  24)
