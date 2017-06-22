(define tagged? (lambda (t) (lambda (e) (and (pair? e) (eq? (car e) t)))))
(define code? (tagged? 'code))
(define code-exp cadr)
(define force-code
  (lambda (f)
    (if (code? f) (code-exp f)
        (error 'force-code (format "expected code, not ~a" f)))))

(define make-let (lambda (e1 e2) `(let ,e1 ,e2)))

(define stFresh 0)
(define stBlock '()) ;; List[Exp]
(define stFun '())   ;; List[(Int,Env,Exp)]

(define reset!
  (lambda ()
    (set! stFresh 0)
    (set! stBlock '())
    (set! stFun '())))

(define run
  (lambda (thunk)
    (let ((sF stFresh)
          (sB stBlock)
          (sN stFun))
      (let ((r (thunk)))
        (set! stFresh sF)
        (set! stBlock sB)
        (set! stFun sN)
        r))))

(define fresh
  (lambda ()
    (set! stFresh (+ 1 stFresh))
    `(var ,(- stFresh 1))))

(define reify
  (lambda (thunk)
    (run (lambda ()
           (set! stBlock '())
           (let ((last (thunk)))
             (fold-right make-let last stBlock))))))

(define reflect
  (lambda (s)
    (set! stBlock (append stBlock (list s)))
    (fresh)))

(define reifyc
  (lambda (thunk)
    (reify (lambda () (force-code (thunk))))))

(define reflectc
  (lambda (s) `(code ,(reflect s))))

(define reifyv
  (lambda (thunk)
    (run (lambda ()
           (set! stBlock '())
           (let ((res (thunk)))
             (if (not (null? stBlock))
                 ;; if we are generating code at all
                 ;; the result must be code
                 (let ((last (force-code res)))
                   `(code ,(fold-right make-let last stBlock)))
                 res))))))


(define findFun
  (lambda (v)
    (let ((env (cadr v))
          (e (caddr v)))
      (define iter
        (lambda (fs)
          (if (null? fs)
              #f
              (let ((f (car fs)))
                (if (and (eq? env (cadr f)) (eq? e (caddr f))) ;; equal?
                    (car f)
                    (iter (cdr fs)))))))
      (iter stFun))))

;; NBE-style reify operator (semantics -> syntax)
(define lift
  (lambda (v)
    (cond
      ((number? v) v)
      ((symbol? v) v)
      (((tagged? 'clo) v)
       (let ((n (findFun v)))
         (if n `(var ,n)
             (let ((env2 (cadr v))
                   (e2 (caddr v)))
               (set! stFun (append stFun (list stFresh env2 e2)))
               (reflect
                `(lambda ,(reify
                      (lambda ()
                        (force-code (evalms
                                     (append env2 `((code ,(fresh)) (code ,(fresh))))
                                     e2))))))))))
      ((code? v) (reflect (lift (force-code v))))
      ((pair? v)
       (let ((a (force-code (car v)))
             (b (force-code (cdr v))))
         (reflect `(cons ,a ,b)))))))

;; this is basically nth!
(define lookup
  (lambda (env n)
    (if (null? env) (error 'lookup "unbound variable")
        (if (= n 0) (car env)
            (lookup (cdr env) (- n 1))))))

(define binary-op
  (lambda (fun)
    (lambda (env e)
      (let ((v1 (evalms env (cadr e)))
            (v2 (evalms env (caddr e))))
        (if (and (code? v1) (code? v2))
            (reflectc (list (car e) (force-code v1) (force-code v2)))
            (if (and (not (code? v1)) (not (code? v2)))
                (fun v1 v2)
                (error 'binary-op "stage error")))))))

(define unary-op
  (lambda (fun)
    (lambda (env e)
      (let ((v1 (evalms env (cadr e))))
        (if (code? v1)
            (reflectc (list (car e) (force-code v1)))
            (fun v1))))))

(define b2n (lambda (r) (if r 1 0)))

(define pred-op
  (lambda (fun)
    (lambda (env e)
      (let ((v1 (evalms env (cadr e))))
        (if (code? v1)
            (reflectc (list (car e) (force-code v1)))
            (b2n (fun v1)))))))

(define evalms
  (lambda (env e)
    (cond
      ((number? e) e)
      ((symbol? e) e)
      (((tagged? 'var) e) (lookup env (cadr e)))
      (((tagged? 'lambda) e) `(clo ,env ,(cadr e)))
      (((tagged? 'let) e)
       (let ((v1 (evalms env (cadr e))))
         (evalms (append env (list v1)) (caddr e))))
      (((tagged? 'lift) e)
       `(code ,(lift (evalms env (cadr e)))))
      (((tagged? 'run) e)
       (let ((v1 (evalms env (cadr e)))
             (thunk (lambda () (evalms env (caddr e)))))
         (if (code? v1)
             (reflectc `(run ,(force-code v1) ,(reifyc thunk)))
             (reifyv (lambda () (evalms '() (force-code (reifyc thunk))))))))
      (((tagged? 'if) e)
       (let ((vc (evalms env (cadr e))))
         (if (code? vc)
             (reflectc `(if ,(force-code vc)
                            ,(reifyc (lambda () (evalms env (caddr e))))
                            ,(reifyc (lambda () (evalms env (cadddr e))))))
             (if (number? vc)
                 (if (not (= vc 0)) (evalms env (caddr e)) (evalms env (cadddr e)))
                 (error 'evalms (format "if expects number for condition, not ~a in ~a" vc e))))))
      (((tagged? '+) e) ((binary-op +) env e))
      (((tagged? '-) e) ((binary-op -) env e))
      (((tagged? '*) e) ((binary-op *) env e))
      (((tagged? 'eq?) e) ((binary-op (lambda (x y) (b2n (eq? x y)))) env e))
      (((tagged? 'car) e) ((unary-op car) env e))
      (((tagged? 'cdr) e) ((unary-op cdr) env e))
      (((tagged? 'number?) e) ((pred-op number?) env e))
      (((tagged? 'symbol?) e) ((pred-op symbol?) env e))
      (((tagged? 'pair?) e) ((pred-op pair?) env e))
      (((tagged? 'code?) e) ((pred-op code?) env e))
      ;; cons is an introduction form, so needs explicit lifting
      (((tagged? 'cons) e) (cons (evalms env (cadr e)) (evalms env (caddr e))))
      (else ;; assume application
       (let ((v1 (evalms env (car e)))
             (v2 (evalms env (cadr e))))
         (if (and (code? v1) (code? v2))
             `(code ,(reflect `(,(force-code v1) ,(force-code v2))))
             (if ((tagged? 'clo) v1)
                 (evalms (append (cadr v1) (list v1 v2)) (caddr v1))
                 (error 'evalms (format "app expects closure, not ~a in ~a" v1 e)))))))))

(define base-fac-anf
'(let (lambda
   (let (if (var 1)
            (let (- (var 1) 1)
            (let ((var 0) (var 2)) (let (* (var 1) (var 3)) (var 4))))
            1)
     (var 2)))
     (var 0)))
