; defining lambdas
(define square (lambda (x) (* x x)))
(define cube (lambda (x) (* x x x)))

; defining vars
(define a 5)
(define b (square a))
(define c (cube a))

; result
(list (list 'square-of a '= b) (list 'cube-of a '= c) (list 'square-of-cube (square (cube a))))