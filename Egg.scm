;;(format #t "Hello world!")

(define (Egg.curry f . xs)
  (lambda ys
    (apply f (append xs ys))))

(define (Egg.constant k)
  (lambda xs
    k))

(define (Egg.compose f . fs)
  (if (null? fs)
      f
      (lambda (x)
	(f
	 ((apply Egg.compose fs)
	  x)))))

(define (Egg.sort-car predicate pairs)
  (sort (lambda (x y)
	  (predicate (car x) (car y)))
	pairs))

(define (Egg.TaggedList? tag v)
  (and (list? v)
       (pair? v)
       (equal? (car v) tag)))

(define (Egg.Token? v)
  (or (Egg.Token.Backslash? v)
      (Egg.Token.Dot? v)
      (Egg.Token.Plus? v)
      (Egg.Token.WS? v)
      (Egg.Token.Id? v)
      (Egg.Token.Int? v)))

(define (Egg.Token->string t)
  (cond
   [(Egg.Token.Backslash? t)
    (Egg.Token.Backslash->string t)]

   [(Egg.Token.Dot? t)
    (Egg.Token.Dot->string t)]
   
   [(Egg.Token.Plus? t)
    (Egg.Token.Plus->string t)]
   
   [(Egg.Token.WS? t)
    (Egg.Token.WS->string t)]
   
   [(Egg.Token.Id? t)
    (Egg.Token.Id->string t)]
   
   [(Egg.Token.Int? t)
    (Egg.Token.Int->string t)]
   
   [else
    (errorf 'Egg.Token->string
	    "No rule for ~a"
	    t)]))

(define Egg.Token.Backslash 'Egg.Token.Backslash)
(define Egg.Token.Backslash? (Egg.curry equal? Egg.Token.Backslash))
(define Egg.Token.Backslash->string (Egg.constant "\\"))

(define Egg.Token.Dot 'Egg.Token.Dot)
(define Egg.Token.Dot? (Egg.curry equal? Egg.Token.Dot))
(define Egg.Token.Dot->string (Egg.constant "."))

(define Egg.Token.Plus 'Egg.Token.Plus)
(define Egg.Token.Plus? (Egg.curry equal? Egg.Token.Plus))
(define Egg.Token.Plus->string (Egg.constant "+"))

(define (Egg.Token.WS str)
  (assert (and (string? str)
	       (for-all char-whitespace? (string->list str))))
  (list 'Egg.Token.WS str))
(define Egg.Token.WS? (Egg.curry Egg.TaggedList? 'Egg.Token.WS))
(define Egg.Token.WS.getStr cadr)
(define Egg.Token.WS->string Egg.Token.WS.getStr)

(define (Egg.Token.Id name)
  (assert (string? name))
  (list 'Egg.Token.Id name))
(define Egg.Token.Id? (Egg.curry Egg.TaggedList? 'Egg.Token.Id))
(define Egg.Token.Id.getName cadr)
(define Egg.Token.Id->string Egg.Token.Id.getName)

(define (Egg.Token.Int value)
  (assert (integer? value))
  (list 'Egg.Token.Int value))
(define Egg.Token.Int? (Egg.curry Egg.TaggedList? 'Egg.Token.Int))
(define Egg.Token.Int.getValue cadr)
(define Egg.Token.Int->string
  (Egg.curry (Egg.compose number->string
			  Egg.Token.Int.getValue)))

(define Egg.Tokens? list?)
(define (Egg.Tokens->string ts)
  (apply string-append (map Egg.Token->string ts)))



(define (Egg.Lexer.lex str)
  (let ([str (string-append str "#")])
    (let loop ([pos 0]
	       [ts '()])
      (if (>= pos (string-length str))
	  (reverse ts)
	  (let ([c (string-ref str pos)])
	    (define (lexBackslash)
	      (if (char=? c #\\)
		  (cons 1
			Egg.Token.Backslash)
		  #f))
	    (define (lexDot)
	      (if (char=? c #\.)
		  (cons 1
			Egg.Token.Dot)
		  #f))
	    (define (lexPlus)
	      (if (char=? c #\+)
		  (cons 1
			Egg.Token.Plus)
		  #f))
	    (define (lexWS)
	      (let loop ([l 0])
		(let ([c (string-ref str (+ pos l))])
		  (if (and (< (+ pos l) (string-length str))
			   (char-whitespace? c))
		      (loop (1+ l))
		    (if (= l 0)
			#f
			(cons l
			      (Egg.Token.WS (substring str
						       pos
						       (+ pos l)))))))))
	  (define (lexId)
	    (let loop ([l 0])
	      (let ([c (string-ref str (+ pos l))])
		(if (and (< (+ pos l) (string-length str))
			 (or (and (= l 0) (or (char-alphabetic? c)
					      (char=? c #\')))
			     (and (> l 0) (or (char-alphabetic? c)
					      (char-numeric? c)
					      (char=? c #\')
					      (char=? c #\-)
					      (char=? c #\_)))))
		    (loop (1+ l))
		    (if (= l 0)
			#f
			(cons l
			      (Egg.Token.Id (substring str
						       pos
						       (+ pos l)))))))))

	  (define (lexInt)
	    (let loop ([l 0])
	      (let ([c (string-ref str (+ pos l))])
		(if (and (< (+ pos l) (string-length str))
			 (char-numeric? c))
		    (loop (1+ l))
		    (if (= l 0)
			#f
			(cons l
			      (Egg.Token.Int (string->number (substring str
									pos
									(+ pos l))))))))))
	    
	  
	  (let* ([lexers (list (lexBackslash)
			       (lexDot)
			       (lexPlus)
			       (lexWS)
			       (lexId)
			       (lexInt))]
		 [successful (filter pair? lexers)]
		 [sorted (Egg.sort-car > successful)]
		 [l (delay (caar sorted))]
		 [t (delay (cdar sorted))])
	    (if (null? sorted)
		(loop (1+ pos)
		      ts)
		(loop (+ pos (force l))
		      (cons (force t) ts)))))))))
			
