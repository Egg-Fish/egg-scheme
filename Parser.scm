(define (Egg.Parser.Return result tokens)
  (unless (or (not result)
	      (Egg.Expr? result))
    (error 'Egg.Parser.Return "Result must be false or an Egg.Expr" result))
  (unless (and (list? tokens)
	       (all Egg.Token? tokens))
    (error 'Egg.Parser.Return "Tokens must be a list of tokens" tokens))
  (pipe (Egg.Object 'Egg.Parser.Return)
	(curry Egg.Object.set ':result result)
	(curry Egg.Object.set ':tokens tokens)))
(define (Egg.Parser.Return? v)
  (and (Egg.Object? v)
       (equal? (Egg.Object.getTag v)
	       'Egg.Parser.Return)))

(define (Egg.Parser.Return.getResult e)
  (unless (Egg.Parser.Return? e)
    (error 'Egg.Parser.Return.getResult "Not an Egg.Parser.Return" e))
  (Egg.Object.get ':result #f e))
(define (Egg.Parser.Return.setResult result e)
  (unless (Egg.Parser.Return? e)
    (error 'Egg.Parser.Return.setResult "Not an Egg.Parser.Return" e))
  (unless (or (not result)
	      (Egg.Expr? result))
    (error 'Egg.Parser.Return.setResult "Result must be false or an Egg.Expr" result))
  (Egg.Object.set ':result result e))

(define (Egg.Parser.Return.getTokens e)
  (unless (Egg.Parser.Return? e)
    (error 'Egg.Parser.Return.getTokens "Not an Egg.Parser.Return" e))
  (Egg.Object.get ':tokens #f e))
(define (Egg.Parser.Return.setTokens tokens e)
  (unless (Egg.Parser.Return? e)
    (error 'Egg.Parser.Return.setTokens "Not an Egg.Parser.Return" e))
  (unless (and (list? tokens)
	       (all Egg.Token? tokens))
    (error 'Egg.Parser.Return.setTokens "Tokens must be a list of tokens" tokens))
  (Egg.Object.set ':tokens tokens e))



(define (Egg.Parser.parseExpr1 tokens)
  (let ([parseLambda (Egg.Parser.parseLambda tokens)])
    (cond
     [(Egg.Parser.Return.getResult parseLambda)
      parseLambda]

     [else
      (Egg.Parser.parseExpr2 tokens)])))

(define (Egg.Parser.parseLambda tokens)
  (if (null? tokens)
      (Egg.Parser.Return #f tokens)
      (let ([lambda (car tokens)])
	(cond
	 [(not (Egg.Token.Lambda? lambda))
	  (Egg.Parser.Return #f tokens)] ;; or parse an Id (with error)

	 [(null? (cdr tokens))
	  (Egg.Parser.Return #f tokens)] ;; Missing dot and body

	 [else
	  (let* ([tokens (cdr tokens)]
		 [dot (car tokens)])
	    (cond
	     [(not (Egg.Token.Dot? dot))
	      (Egg.Parser.Return #f tokens)] ;; or parse without dot (with error)

	     [(null? (cdr tokens))
	      (Egg.Parser.Return #f tokens)] ;; Missing body

	     [else
	      (let* ([tokens (cdr tokens)]
		     [parseBody (Egg.Parser.parseExpr1 tokens)]
		     [body (Egg.Parser.Return.getResult parseBody)]
		     [tokens (Egg.Parser.Return.getTokens parseBody)])
		(if (not body)
		    (Egg.Parser.Return #f tokens)
		    (Egg.Parser.Return (Egg.Expr.Lambda (Egg.Expr.Var (Egg.Token.Lambda.getParameter lambda))
							body)
				       tokens)))]))]))))


(define (Egg.Parser.parseExpr2 tokens)
  (let ([parseBinop1 (Egg.Parser.parseBinop1 tokens)])
    (cond
     [(Egg.Parser.Return.getResult parseBinop1)
      parseBinop1]

     [else
      (Egg.Parser.parseExpr3 tokens)])))

(define (Egg.Parser.parseBinop1 tokens)
  (if (null? tokens)
      (Egg.Parser.Return #f tokens)
      (let* ([parseLhs (Egg.Parser.parseBinop2 tokens)]
	     [lhs (Egg.Parser.Return.getResult parseLhs)]
	     [tokens (Egg.Parser.Return.getTokens parseLhs)])
	(if (not lhs)
	    (Egg.Parser.Return #f tokens)
	    (let loop ([e lhs]
		       [tokens tokens])
	      (if (null? tokens)
		  (Egg.Parser.Return e tokens)
		  (let* ([t (car tokens)]
			 [op (cond
			      [(Egg.Token.Plus? t)
			       'Add]
			      [else
			       #f])])
		    (if (not op)
			(Egg.Parser.Return e tokens)
			(let* ([tokens (cdr tokens)]
			       [parseRhs (Egg.Parser.parseBinop2 tokens)]
			       [rhs (Egg.Parser.Return.getResult parseRhs)]
			       [tokens (Egg.Parser.Return.getTokens parseRhs)])
			  (if (not rhs)
			      (Egg.Parser.Return #f tokens)
			      (loop (Egg.Expr.Binop op e rhs)
				    tokens)))))))))))

(define (Egg.Parser.parseBinop2 tokens)
  (if (null? tokens)
      (Egg.Parser.Return #f tokens)
      (let* ([parseLhs (Egg.Parser.parseBinop3 tokens)]
	     [lhs (Egg.Parser.Return.getResult parseLhs)]
	     [tokens (Egg.Parser.Return.getTokens parseLhs)])
	(if (not lhs)
	    (Egg.Parser.Return #f tokens)
	    (let loop ([e lhs]
		       [tokens tokens])
	      (if (null? tokens)
		  (Egg.Parser.Return e tokens)
		  (let* ([t (car tokens)]
			 [op (cond
			      [(Egg.Token.Star? t)
			       'Mul]
			      [else
			       #f])])
		    (if (not op)
			(Egg.Parser.Return e tokens)
			(let* ([tokens (cdr tokens)]
			       [parseRhs (Egg.Parser.parseBinop3 tokens)]
			       [rhs (Egg.Parser.Return.getResult parseRhs)]
			       [tokens (Egg.Parser.Return.getTokens parseRhs)])
			  (if (not rhs)
			      (Egg.Parser.Return #f tokens)
			      (loop (Egg.Expr.Binop op e rhs)
				    tokens)))))))))))

(define (Egg.Parser.parseBinop3 tokens)
  (Egg.Parser.parseExpr3 tokens))


(define (Egg.Parser.parseExpr3 tokens)
  (let ([parseApp (Egg.Parser.parseApp tokens)])
    (cond
     [(Egg.Parser.Return.getResult parseApp)
      parseApp]

     [else
      (Egg.Parser.parseExpr4 tokens)])))

(define (Egg.Parser.parseApp tokens)
  (if (null? tokens)
      (Egg.Parser.Return #f tokens)
      (let* ([parseLhs (Egg.Parser.parseExpr4 tokens)]
	     [lhs (Egg.Parser.Return.getResult parseLhs)]
	     [tokens (Egg.Parser.Return.getTokens parseLhs)])
	(if (not lhs)
	    (Egg.Parser.Return #f tokens)
	    (let* ([parseRhs (Egg.Parser.parseExpr4 tokens)]
		   [rhs (Egg.Parser.Return.getResult parseRhs)]
		   [tokens (Egg.Parser.Return.getTokens parseRhs)])
	      (if (not rhs)
		  (Egg.Parser.Return #f tokens)
		  (let loop ([e (Egg.Expr.App lhs rhs)]
			     [tokens tokens])
		    (let* ([parseRhs (Egg.Parser.parseExpr4 tokens)]
			   [rhs (Egg.Parser.Return.getResult parseRhs)]
			   [tokens (Egg.Parser.Return.getTokens parseRhs)])
		      (if (not rhs)
			  (Egg.Parser.Return e tokens)
			  (loop (Egg.Expr.App e rhs)
				tokens))))))))))


(define (Egg.Parser.parseExpr4 tokens)
  (let ([parseVar (Egg.Parser.parseVar tokens)]
	[parseInt (Egg.Parser.parseInt tokens)])
    (cond
     [(Egg.Parser.Return.getResult parseVar)
      parseVar]

     [(Egg.Parser.Return.getResult parseInt)
      parseInt]

     [else
      (Egg.Parser.parseExpr5 tokens)])))

(define (Egg.Parser.parseVar tokens)
  (if (or (null? tokens)
	  (not (Egg.Token.Id? (car tokens))))
      (Egg.Parser.Return #f tokens)
      (Egg.Parser.Return (Egg.Expr.Var (Egg.Token.Id.getName (car tokens)))
			 (cdr tokens))))

(define (Egg.Parser.parseInt tokens)
  (if (or (null? tokens)
	  (not (Egg.Token.Int? (car tokens))))
      (Egg.Parser.Return #f tokens)
      (Egg.Parser.Return (Egg.Expr.Int (Egg.Token.Int.getValue (car tokens)))
			 (cdr tokens))))


(define (Egg.Parser.parseExpr5 tokens)
  (let ([parseParens (Egg.Parser.parseParens tokens)])
    (cond
     [(Egg.Parser.Return.getResult parseParens)
      parseParens]

     [else
      (Egg.Parser.Return #f tokens)])))

(define (Egg.Parser.parseParens tokens)
  (if (null? tokens)
      (Egg.Parser.Return #f tokens)
      (let* ([lp (car tokens)]
	     [tokens (cdr tokens)])
	(if (not (Egg.Token.LP? lp))
	    (Egg.Parser.Return #f tokens) ;; Missing (
	    (let* ([parseExpr (Egg.Parser.parseExpr1 tokens)]
		   [expr (Egg.Parser.Return.getResult parseExpr)]
		   [tokens (Egg.Parser.Return.getTokens parseExpr)])
	      (if (null? tokens)
		  (Egg.Parser.Return #f tokens) ;; Missing )
		  (let* ([rp (car tokens)]
			 [tokens (cdr tokens)])
		    (if (not (Egg.Token.RP? rp))
			(Egg.Parser.Return #f tokens) ;; Missing ) (or maybe unit expr)
			(Egg.Parser.Return expr tokens)))))))))
