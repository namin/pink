(define matcher-src
 '(let star_loop (lambda star_loop m (lambda _ c (maybe-lift (lambda _ s
  (if (eq?  (maybe-lift 'yes) (m s)) (maybe-lift 'yes)
  (if (eq?  (maybe-lift 'done) (car s)) (maybe-lift 'no)
  (if (eq?  '_ c) (((star_loop m) c) (cdr s))
  (if (eq?  (maybe-lift c) (car s)) (((star_loop m) c) (cdr s)) (maybe-lift 'no)))))))))
(let match_here (lambda match_here r (lambda _ s (if (eq?  'done (car r)) (maybe-lift 'yes)
  (let m (lambda _ s
      (if (eq?  '_ (car r)) (if (eq?  (maybe-lift 'done) (car s)) (maybe-lift 'no) ((match_here (cdr r)) (cdr s)))
      (if (eq?  (maybe-lift 'done) (car s)) (maybe-lift 'no)
      (if (eq?  (maybe-lift (car r)) (car s)) ((match_here (cdr r)) (cdr s)) (maybe-lift 'no)))))
    (if (eq?  'done (car (cdr r))) (m s)
    (if (eq?  '* (car (cdr r))) (((star_loop (match_here (cdr (cdr r)))) (car r)) s) (m s)))))))
(let match (lambda match r
  (if (eq?  'done (car r)) (maybe-lift (lambda _ s (maybe-lift 'yes))) (maybe-lift (match_here r))))
match)))
)
