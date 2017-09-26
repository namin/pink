;; from https://github.com/jasonhemann/microKanren/blob/master/microKanren.scm
;; see also http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

(define mk (lambda (program)
`(let = (lambda _ a (lambda _ b (if (- a b) 0 1)))
(let assp (lambda assp p (lambda _ s (if (pair? s) (if (p (car (car s))) (car s) ((assp p) (cdr s))) s)))
(let var (lambda _ c (cons 'var c))
(let var? (lambda _ x (if (pair? x) (if (symbol? (car x)) (eq? 'var (car x)) 0) 0))
(let var=? (lambda _ x1 (lambda _ x2 ((= (cdr x1)) (cdr x2))))
(let walk (lambda walk u (lambda _ s
  (let pr (if (var? u) ((assp (var=? u)) s) '())
    (if (null? pr) u ((walk (cdr pr)) s)))))
(let ext-s (lambda _ x (lambda _ v (lambda _ s (cons (cons x v) s))))
(let mzero '()
(let unit (lambda _ s/c (cons s/c mzero))
(let unify (lambda unify u (lambda _ v (lambda _ s
  (let u ((walk u) s)
  (let v ((walk v) s)
  (if (if (var? u) (if (var? v) ((var=? u) v) 0) 0) s
  (if (var? u) (((ext-s u) v) s)
  (if (var? v) (((ext-s v) u) s)
  (if (if (pair? u) (pair? v) 0)
      (let s (((unify (car u)) (car v)) s)
         (if (pair? s) (((unify (cdr u)) (cdr v)) s) 0))
  (if (eq? u v) s '()))))))))))
(let == (lambda _ u (lambda _ v (lambda _ s/c
    (let s (((unify u) v) (car s/c))
      (if (pair? s) (unit (cons s (cdr s/c))) mzero)))))
(let call/fresh (lambda _ f (lambda _ s/c
    (let c (cdr s/c)
      ((f (var c)) (cons (car s/c) (+ c 1))))))
(let mplus (lambda mplus $1 (lambda _ $2
  (if (null? $1) $2
  (if (pair? $1) (cons (car $1) ((mplus (cdr $1)) $2))
   ;; (procedure? $1)
   (lambda _ _ ((mplus $2) ($1 0)))))))
(let bind (lambda bind $ (lambda _ g
  (if (null? $) mzero
  (if (pair? $) ((mplus (g (car $))) ((bind (cdr $)) g))
  ;; (procedure? $)
  (lambda _ _ ((bind ($ 0)) g))))))
(let disj (lambda _ g1 (lambda _ g2 (lambda _ s/c ((mplus (g1 s/c)) (g2 s/c)))))
(let conj (lambda _ g1 (lambda _ g2 (lambda _ s/c ((bind (g1 s/c)) g2))))
(let empty-state (cons '() 0)
,program
)))))))))))))))))
))
