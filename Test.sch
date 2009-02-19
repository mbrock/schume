
(callcc (lambda (k) ((lambda (x) (x x))
		     (lambda (y) (y k)))))
