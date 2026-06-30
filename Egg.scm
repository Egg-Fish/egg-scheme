(define (curry f . xs)
  (lambda ys
    (apply f (append xs ys))))

(define (compose f . fs)
  (if (null? fs)
      f
      (lambda (x)
	(f ((apply compose fs) x)))))

(define (pipe v . fs)
  ((apply compose (reverse fs)) v))


(define (all predicate xs)
  (or (null? xs)
      (and (predicate (car xs))
	   (all predicate (cdr xs)))))

(define (any predicate xs)
  (and (not (null? xs))
       (or (predicate (car xs))
	   (any predicate (cdr xs)))))

(define (lookup predicate default xs)
  (if (null? xs)
      default
      (let ([x (car xs)])
	(if (predicate x)
	    x
	    (lookup predicate default (cdr xs))))))

(define (replace old new xs)
  (map (lambda (x)
	 (if (equal? x old)
	     new
	     x))
       xs))

(define (in v xs)
  (not (not (member v xs))))

(load "Object.scm")

(load "Token.scm")
(load "Expr.scm")

(load "Lexer.scm")
(load "Parser.scm")
