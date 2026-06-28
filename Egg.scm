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

;;; Tokens
;;; *-----------*---------*
;;; | Token     | Example |
;;; |-----------|---------|
;;; | Backslash | \       |
;;; | Dot       | .       |
;;; | Plus      | +       |
;;; | LP        | (       |
;;; | RP        | )       |
;;; | WS        |         |
;;; | Id        | xyz     |
;;; | Int       | 42      |
;;; *-----------*---------*

(define (Egg.Token? v)
  (or (Egg.Token.Backslash? v)
      (Egg.Token.Dot? v)
      (Egg.Token.Plus? v)
      (Egg.Token.LP? v)
      (Egg.Token.RP? v)
      (Egg.Token.WS? v)
      (Egg.Token.Id? v)
      (Egg.Token.Int? v)
      (Egg.Token.Unknown? v)))

(define (Egg.Token->string t)
  (cond
   [(Egg.Token.Backslash? t)
    (Egg.Token.Backslash->string t)]

   [(Egg.Token.Dot? t)
    (Egg.Token.Dot->string t)]

   [(Egg.Token.Plus? t)
    (Egg.Token.Plus->string t)]

   [(Egg.Token.LP? t)
    (Egg.Token.LP->string t)]

   [(Egg.Token.RP? t)
    (Egg.Token.RP->string t)]

   [(Egg.Token.WS? t)
    (Egg.Token.WS->string t)]

   [(Egg.Token.Id? t)
    (Egg.Token.Id->string t)]

   [(Egg.Token.Int? t)
    (Egg.Token.Int->string t)]

   [(Egg.Token.Unknown? t)
    (Egg.Token.Unknown->string t)]

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

(define Egg.Token.LP 'Egg.Token.LP)
(define Egg.Token.LP? (Egg.curry equal? Egg.Token.LP))
(define Egg.Token.LP->string (Egg.constant "("))

(define Egg.Token.RP 'Egg.Token.RP)
(define Egg.Token.RP? (Egg.curry equal? Egg.Token.RP))
(define Egg.Token.RP->string (Egg.constant ")"))

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

(define (Egg.Token.Unknown char)
  (assert (char? char))
  (list 'Egg.Token.Unknown char))
(define Egg.Token.Unknown? (Egg.curry Egg.TaggedList? 'Egg.Token.Unknown))
(define Egg.Token.Unknown.getChar cadr)
(define Egg.Token.Unknown->string (Egg.compose string Egg.Token.Unknown.getChar))

(define Egg.Tokens? list?)
(define (Egg.Tokens->string ts)
  (apply string-append (map Egg.Token->string ts)))

(define (Egg.Tokens.check pred ts)
  (and (not (null? ts))
       (pred (car ts))))

(define (Egg.Tokens.consume pred ts)
  (if (not (Egg.Tokens.check pred ts))
      ts
      (Egg.Tokens.consume pred (cdr ts))))

(define Egg.Tokens.consumeWS (Egg.curry Egg.Tokens.consume Egg.Token.WS?))


(define (Egg.Lexer.lex str)
  (let* ([endMarker #\~]
	 [str (string-append str (string endMarker))])
    (let loop ([pos 0]
	       [ts '()])
      (if (= pos (string-length str))
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

	    (define (lexLP)
	      (if (char=? c #\()
		  (cons 1
			Egg.Token.LP)
		  #f))

	    (define (lexRP)
	      (if (char=? c #\))
		  (cons 1
			Egg.Token.RP)
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


	    (let* ([c (string-ref str pos)]
		   [lexers (list (lexBackslash)
				 (lexDot)
				 (lexPlus)
				 (lexLP)
				 (lexRP)
				 (lexWS)
				 (lexId)
				 (lexInt))]
		   [successful (filter pair? lexers)]
		   [sorted (Egg.sort-car > successful)]
		   [l (delay (caar sorted))]
		   [t (delay (cdar sorted))])
	      (if (null? sorted)
		  (if (char=? c endMarker)
		      (loop (1+ pos)
			    ts)
		      (loop (1+ pos)
			    (cons (Egg.Token.Unknown c) ts)))
		  (loop (+ pos (force l))
			(cons (force t) ts)))))))))


(define (Egg.Lexer.lexFile filename)
  (call-with-input-file filename
    (lambda (f)
      (let loop ([s '()])
	(let ([c (read-char f)])
	  (if (eof-object? c)
	      (let ([str (list->string (reverse s))])
		(Egg.Lexer.lex str))
	      (loop (cons c s))))))))



(define (Egg.Parser.parse ts) (Egg.Parser.parseExpr1 ts))

(define (Egg.Parser.parseExpr1 ts)
  (let* ([ts (Egg.Tokens.consumeWS ts)]
	 [parseLambda (Egg.Parser.parseLambda ts)])
    (cond
     [(and (pair? ts)
	   (Egg.Token.Backslash? (car ts)))
      parseLambda]

     [else
      (Egg.Parser.parseExpr2 ts)])))

(define (Egg.Parser.parseLambda ts)
  (if (null? ts)
      (list #f
	    `("ERROR: EMPTY")
	    ts)
      (let* ([bs (car ts)]
	     [err1 (if (Egg.Token.Backslash? bs) `() `("ERROR: Missing backslash"))]
	     [ts (cdr ts)])
	(if (null? ts)
	      (list #f
		    `("ERROR: Missing param")
		    ts)
	      (let* ([param (Egg.Parser.parseVar ts)]
		     [err2 (if (car param) `() `("ERROR: Invalid param" . ,(cadr param)))]
		     [ts (Egg.Tokens.consume (Egg.compose not Egg.Token.Dot?) (caddr param))])
		(if (null? ts)
		    (list #f
			  `("ERROR: Missing dot")
			  ts)
		    (let* ([body (Egg.Parser.parseExpr1 (cdr ts))]
			   [err3 (if (car body) `() `("ERROR: Invalid body" . ,(cadr body)))])
		      (if (and (null? err1)
			       (null? err2)
			       (null? err3))
			  (list `(Expr.Lambda ,(car param) ,(car body))
				(append (cadr param) (cadr body))
				(caddr body))
			  (list #f
				(append err1 err2 err3)
				(caddr body))))))))))




(define (Egg.Parser.parseExpr2 ts)
  (let* ([ts (Egg.Tokens.consumeWS ts)]
	 [parseBinop (Egg.Parser.parseBinop ts)])
    (cond
     [(car parseBinop)
      parseBinop]

     [else
      (Egg.Parser.parseExpr3 ts)])))


(define (Egg.Parser.parseBinop ts) (Egg.Parser.parseBinop1 ts))

(define (Egg.Parser.parseBinop1 ts)
  (let* ([ts (Egg.Tokens.consumeWS ts)]
	 [lhs (Egg.Parser.parseBinop2 ts)])
    (if (not (car lhs))
	lhs
	(let loop ([expr (car lhs)]
		   [errs (cadr lhs)]
		   [ts (Egg.Tokens.consumeWS (caddr lhs))])
	  (cond
	   [(null? ts)
	    (list expr
		  errs
		  ts)]

	   [(Egg.Token.Plus? (car ts))
	    (let ([rhs (Egg.Parser.parseBinop2 (cdr ts))])
	      (loop `(Expr.Binop Add ,expr ,(car rhs))
		    (append errs (cadr rhs))
		    (Egg.Tokens.consumeWS (caddr rhs))))]

	   [else
	    (list expr
		  errs
		  ts)])))))


(define (Egg.Parser.parseBinop2 ts) (Egg.Parser.parseExpr3 ts))

(define (Egg.Parser.parseExpr3 ts)
  (let* ([ts (Egg.Tokens.consumeWS ts)]
	 [lhs (Egg.Parser.parseExpr4 ts)])
    (if (not (car lhs))
	lhs
	(let loop ([expr (car lhs)]
		   [errs (cadr lhs)]
		   [ts (Egg.Tokens.consumeWS (caddr lhs))])
	  (let ([rhs (Egg.Parser.parseExpr4 ts)])
	    (if (not (car rhs))
		(list expr
		      errs
		      ts)
		(loop `(Expr.App ,expr ,(car rhs))
		      (append errs (cadr rhs))
		      (Egg.Tokens.consumeWS (caddr rhs)))))))))

(define (Egg.Parser.parseExpr4 ts)
  (let* ([ts (Egg.Tokens.consumeWS ts)]
	 [parseInt (Egg.Parser.parseInt ts)]
	 [parseVar (Egg.Parser.parseVar ts)])
    (cond
     [(car parseInt)
      parseInt]

     [(car parseVar)
      parseVar]

     [else
      (Egg.Parser.parseExpr5 ts)])))

(define (Egg.Parser.parseInt ts)
  (if (null? ts)
      (list #f
	    `("ERROR: EMPTY")
	    ts)
      (let ([t (car ts)]
	    [tss (cdr ts)])
	(if (Egg.Token.Int? t)
	    (list `(Expr.Int ,(Egg.Token.Int.getValue (car ts)))
		  `()
		  tss)
	    (list #f
		  `("ERROR: Not an int")
		  ts)))))

(define (Egg.Parser.parseVar ts)
  (if (null? ts)
      (list #f
	    `("ERROR: EMPTY")
	    ts)
      (let ([t (car ts)]
	    [tss (cdr ts)])
	(if (Egg.Token.Id? t)
	    (list `(Expr.Var ,(Egg.Token.Id.getName (car ts)))
		  `()
		  tss)
	    (list #f
		  `("ERROR: Not a variable")
		  ts)))))

(define (Egg.Parser.parseExpr5 ts)
  (let ([ts (Egg.Tokens.consumeWS ts)])
    (if (null? ts)
	(list #f
	      `("ERROR: EMPTY")
	      ts)
	(let ([lp (car ts)])
	  (if (not (Egg.Token.LP? lp))
	      (list #f
		    `("ERROR: Could not parse as expression")
		    ts)
	      (let* ([expr (Egg.Parser.parseExpr1 (cdr ts))]
		     [ts (Egg.Tokens.consumeWS (caddr expr))])
		(if (null? ts)
		    (list #f
			  `("ERROR: Missing closing parenthesis")
			  ts)
		    (let ([rp (car ts)])
		      (if (not (Egg.Token.RP? rp))
			  (list #f
				`("ERROR: Missing closing parenthesis")
				ts)
			  (list (car expr)
				(cadr expr)
				(cdr ts)))))))))))
