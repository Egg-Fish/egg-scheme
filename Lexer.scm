(define (Egg.Lexer.Return result position)
  (unless (or (not result)
	      (Egg.Token? result))
    (error 'Egg.Lexer.Return "Result must be false or an Egg.Token" result))
  (unless (and (integer? position)
	       (<= 0 position))
    (error 'Egg.Lexer.Return "Position must be a nonnegative integer" position))
  (pipe (Egg.Object 'Egg.Lexer.Return)
	(curry Egg.Object.set ':result result)
	(curry Egg.Object.set ':position position)))
(define (Egg.Lexer.Return? v)
  (and (Egg.Object? v)
       (equal? (Egg.Object.getTag v)
	       'Egg.Lexer.Return)))

(define (Egg.Lexer.Return.getResult t)
  (unless (Egg.Lexer.Return? t)
    (error 'Egg.Lexer.Return.getResult "Not an Egg.Lexer.Return" t))
  (Egg.Object.get ':result #f t))
(define (Egg.Lexer.Return.setResult result t)
  (unless (Egg.Lexer.Return? t)
    (error 'Egg.Lexer.Return.setResult "Not an Egg.Lexer.Return" t))
  (unless (or (not result)
	      (Egg.Token? result))
    (error 'Egg.Lexer.Return.setResult "Result must be false or an Egg.Token" result))
  (Egg.Object.set ':result result t))

(define (Egg.Lexer.Return.getPosition t)
  (unless (Egg.Lexer.Return? t)
    (error 'Egg.Lexer.Return.getPosition "Not an Egg.Lexer.Return" t))
  (Egg.Object.get ':position #f t))
(define (Egg.Lexer.Return.setPosition position t)
  (unless (Egg.Lexer.Return? t)
    (error 'Egg.Lexer.Return.setPosition "Not an Egg.Lexer.Return" t))
  (unless (integer? position)
    (error 'Egg.Lexer.Return.setPosition "Position must be an integer" position))
  (Egg.Object.set ':position position t))



(define (Egg.Lexer.lexPlus string position)
  (if (or (>= position (string-length string))
	  (not (char=? (string-ref string position) #\+)))
      (Egg.Lexer.Return #f position)
      (Egg.Lexer.Return (Egg.Token.Plus) (+ position 1))))


(define (Egg.Lexer.lexStar string position)
  (if (or (>= position (string-length string))
	  (not (char=? (string-ref string position) #\*)))
      (Egg.Lexer.Return #f position)
      (Egg.Lexer.Return (Egg.Token.Star) (+ position 1))))


(define (Egg.Lexer.lexDot string position)
  (if (or (>= position (string-length string))
	  (not (char=? (string-ref string position) #\.)))
      (Egg.Lexer.Return #f position)
      (Egg.Lexer.Return (Egg.Token.Dot) (+ position 1))))


(define (Egg.Lexer.lexLP string position)
  (if (or (>= position (string-length string))
	  (not (char=? (string-ref string position) #\()))
      (Egg.Lexer.Return #f position)
      (Egg.Lexer.Return (Egg.Token.LP) (+ position 1))))


(define (Egg.Lexer.lexRP string position)
  (if (or (>= position (string-length string))
	  (not (char=? (string-ref string position) #\))))
      (Egg.Lexer.Return #f position)
      (Egg.Lexer.Return (Egg.Token.RP) (+ position 1))))


(define (Egg.Lexer.lexInt string position)
  (if (or (>= position (string-length string))
	  (not (char-numeric? (string-ref string position))))
      (Egg.Lexer.Return #f position)
      (let loop ([value 0]
		 [p position])
	(if (>= p (string-length string))
	    (Egg.Lexer.Return (Egg.Token.Int value) p)
	    (let ([c (string-ref string p)])
	      (if (not (char-numeric? c))
		  (Egg.Lexer.Return (Egg.Token.Int value) p)
		  (loop (+ (* value 10)
			   (- (char->integer c)
			      (char->integer #\0)))
			(+ p 1))))))))


(define (Egg.Lexer.lexId string position)
  (if (or (>= position (string-length string))
	  (not (char-alphabetic? (string-ref string position))))
      (Egg.Lexer.Return #f position)
      (let loop ([p position])
	(if (>= p (string-length string))
	    (Egg.Lexer.Return (Egg.Token.Id (substring string position p)) p)
	    (let ([c (string-ref string p)])
	      (if (not (or (char-alphabetic? c)
			   (char-numeric? c)))
		  (Egg.Lexer.Return (Egg.Token.Id (substring string position p)) p)
		  (loop (+ p 1))))))))


(define (Egg.Lexer.lexLambda string position)
  (if (or (>= position (string-length string))
	  (not (char=? (string-ref string position) #\\)))
      (Egg.Lexer.Return #f position)
      (let* ([return (Egg.Lexer.lexId string (+ position 1))]
	     [parameter (Egg.Lexer.Return.getResult return)])
	(if (not parameter)
	    (Egg.Lexer.Return #f position)
	    (Egg.Lexer.Return (Egg.Token.Lambda (Egg.Token.Id.getName parameter))
			      (Egg.Lexer.Return.getPosition return))))))


(define (Egg.Lexer.lex string)
  (unless (string? string)
    (error 'Egg.Lexer.lex "String must be a string" string))
  (let lex ([position 0])
    (if (>= position (string-length string))
	'()
	(let* ([position (let loop ([p position])
			   (if (char-whitespace? (string-ref string p))
			       (loop (+ p 1))
			       p))]
	       [lexers (list Egg.Lexer.lexPlus
			     Egg.Lexer.lexStar
			     Egg.Lexer.lexDot
			     Egg.Lexer.lexLP
			     Egg.Lexer.lexRP
			     Egg.Lexer.lexInt
			     Egg.Lexer.lexId
			     Egg.Lexer.lexLambda)]
	       [returns (map (lambda (l) (l string position)) lexers)]
	       [returns (filter Egg.Lexer.Return.getResult returns)])
	  (if (null? returns)
	      (cons (Egg.Token.Unknown (string-ref string position))
		    (lex (+ position 1)))
	      (let* ([returns (sort (lambda (l r)
				      (> (Egg.Lexer.Return.getPosition l)
					 (Egg.Lexer.Return.getPosition r)))
				    returns)]
		     [return (car returns)]
		     [token (Egg.Lexer.Return.getResult return)]
		     [position (Egg.Lexer.Return.getPosition return)])
		(cons token (lex position))))))))

(define (Egg.Lexer.lexFile filename)
  (unless (string? filename)
    (error 'Egg.Lexer.lexFile "Filename must be a string" filename))
  (call-with-input-file filename
    (lambda (f)
      (let loop ([s (list)])
	(let ([c (read-char f)])
	  (if (eof-object? c)
	      (let ([string (list->string (reverse s))])
		(Egg.Lexer.lex string))
	      (loop (cons c s))))))))
