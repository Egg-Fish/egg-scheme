(define (Egg.Parser.Return result errors tokens)
  (unless (or (not result)
	      (Egg.Expr? result))
    (error 'Egg.Parser.Return "Result must be false or an Egg.Expr" result))
  (unless (and (list? errors)
	       (all list? errors))
    (error 'Egg.Parser.Return "Errors must be a list of lists" errors))
  (unless (and (list? tokens)
	       (all Egg.Token? tokens))
    (error 'Egg.Parser.Return "Tokens must be a list of tokens" tokens))
  (pipe (Egg.Object 'Egg.Parser.Return)
	(curry Egg.Object.set ':result result)
	(curry Egg.Object.set ':errors errors)
	(curry Egg.Object.set ':tokens tokens)))
(define (Egg.Parser.Return? v)
  (and (Egg.Object? v)
       (equal? (Egg.Object.getTag v)
	       'Egg.Parser.Return)))

(define (Egg.Parser.Return.getResult ret)
  (unless (Egg.Parser.Return? ret)
    (error 'Egg.Parser.Return.getResult "Not an Egg.Parser.Return" ret))
  (Egg.Object.get ':result #f ret))
(define (Egg.Parser.Return.setResult result ret)
  (unless (Egg.Parser.Return? ret)
    (error 'Egg.Parser.Return.setResult "Not an Egg.Parser.Return" ret))
  (unless (or (not result)
	      (Egg.Expr? result))
    (error 'Egg.Parser.Return.setResult "Result must be false or an Egg.Expr" result))
  (Egg.Object.set ':result result ret))

(define (Egg.Parser.Return.getErrors ret)
  (unless (Egg.Parser.Return? ret)
    (error 'Egg.Parser.Return.getErrors "Not an Egg.Parser.Return" ret))
  (Egg.Object.get ':errors #f ret))
(define (Egg.Parser.Return.setErrors errors ret)
  (unless (Egg.Parser.Return? ret)
    (error 'Egg.Parser.Return.setErrors "Not an Egg.Parser.Return" ret))
  (unless (and (list? errors)
	       (all list? errors))
    (error 'Egg.Parser.Return.setErrors "Errors must be a list of lists" errors))
  (Egg.Object.set ':errors errors ret))
(define (Egg.Parser.Return.addError err ret)
  (Egg.Parser.Return.setErrors (cons err (Egg.Parser.Return.getErrors ret))
			       ret))

(define (Egg.Parser.Return.getTokens ret)
  (unless (Egg.Parser.Return? ret)
    (error 'Egg.Parser.Return.getTokens "Not an Egg.Parser.Return" ret))
  (Egg.Object.get ':tokens #f ret))
(define (Egg.Parser.Return.setTokens tokens ret)
  (unless (Egg.Parser.Return? ret)
    (error 'Egg.Parser.Return.setTokens "Not an Egg.Parser.Return" ret))
  (unless (and (list? tokens)
	       (all Egg.Token? tokens))
    (error 'Egg.Parser.Return.setTokens "Tokens must be a list of tokens" tokens))
  (Egg.Object.set ':tokens tokens ret))

(define (Egg.Parser.Return.returnUnexpectedEOF tokens)
  (Egg.Parser.Return #f `(("Unexpected end of file" ,tokens)) tokens))

(define (Egg.Parser.Return.returnUnexpectedToken expected tokens)
  (Egg.Parser.Return #f `(("Expected" ,expected "Got" ,(car tokens))) tokens))


(define (Egg.Parser.parseExpr1 tokens)
  (let ([parseLambda (Egg.Parser.parseLambda tokens)])
    (cond
     [(Egg.Parser.Return.getResult parseLambda)
      parseLambda]

     [else
      (Egg.Parser.parseExpr2 tokens)])))

(define (Egg.Parser.parseLambda tokens)
  (if (null? tokens)
      (Egg.Parser.Return.returnUnexpectedEOF tokens)
      (let ([lambda (car tokens)])
	(cond
	 [(not (Egg.Token.Lambda? lambda))
	  (Egg.Parser.Return.returnUnexpectedToken 'Egg.Token.Lambda
						   tokens)] ;; or parse an Id (with error)

	 [(null? (cdr tokens))
	  (Egg.Parser.Return.returnUnexpectedEOF (cdr tokens))] ;; Missing dot and body

	 [else
	  (let* ([tokens (cdr tokens)]
		 [dot (car tokens)])
	    (cond
	     [(not (Egg.Token.Dot? dot))
	      (Egg.Parser.Return.returnUnexpectedToken (Egg.Token.Dot)
						       tokens)] ;; or parse without dot (with error)

	     [(null? (cdr tokens))
	      (Egg.Parser.Return.returnUnexpectedEOF (cdr tokens))] ;; Missing body

	     [else
	      (let* ([tokens (cdr tokens)]
		     [parseBody (Egg.Parser.parseExpr1 tokens)]
		     [body (Egg.Parser.Return.getResult parseBody)]
		     [errors (Egg.Parser.Return.getErrors parseBody)]
		     [tokens (Egg.Parser.Return.getTokens parseBody)])
		(if (not body)
		    (Egg.Parser.Return.addError `("Could not parse body") parseBody)
		    (Egg.Parser.Return (Egg.Expr.Lambda (Egg.Expr.Var (Egg.Token.Lambda.getParameter lambda))
							body)
				       errors
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
      (Egg.Parser.Return.returnUnexpectedEOF tokens)
      (let* ([parseLhs (Egg.Parser.parseBinop2 tokens)]
	     [lhs (Egg.Parser.Return.getResult parseLhs)]
	     [errors (Egg.Parser.Return.getErrors parseLhs)]
	     [tokens (Egg.Parser.Return.getTokens parseLhs)])
	(if (not lhs)
	    (Egg.Parser.Return.addError `("Could not parse lhs") parseLhs)
	    (let loop ([e lhs]
		       [errors errors]
		       [tokens tokens])
	      (if (null? tokens)
		  (Egg.Parser.Return e errors tokens)
		  (let* ([t (car tokens)]
			 [op (cond
			      [(Egg.Token.Plus? t)
			       'Add]
			      [else
			       #f])])
		    (if (not op)
			(Egg.Parser.Return e errors tokens)
			(let ([tokens (cdr tokens)])
			  (if (null? tokens)
			      (Egg.Parser.Return.returnUnexpectedEOF tokens)
			      (let* ([parseRhs (Egg.Parser.parseBinop2 tokens)]
				     [rhs (Egg.Parser.Return.getResult parseRhs)]
				     [tokens (Egg.Parser.Return.getTokens parseRhs)])
				(if (not rhs)
				    (Egg.Parser.Return.addError `("Could not parse rhs") parseRhs)
				    (loop (Egg.Expr.Binop op e rhs)
					  (append (Egg.Parser.Return.getErrors parseRhs) errors)
					  tokens)))))))))))))

(define (Egg.Parser.parseBinop2 tokens)
  (if (null? tokens)
      (Egg.Parser.Return.returnUnexpectedEOF tokens)
      (let* ([parseLhs (Egg.Parser.parseBinop3 tokens)]
	     [lhs (Egg.Parser.Return.getResult parseLhs)]
	     [errors (Egg.Parser.Return.getErrors parseLhs)]
	     [tokens (Egg.Parser.Return.getTokens parseLhs)])
	(if (not lhs)
	    (Egg.Parser.Return.addError `("Could not parse lhs") parseLhs)
	    (let loop ([e lhs]
		       [errors errors]
		       [tokens tokens])
	      (if (null? tokens)
		  (Egg.Parser.Return e errors tokens)
		  (let* ([t (car tokens)]
			 [op (cond
			      [(Egg.Token.Star? t)
			       'Mul]
			      [else
			       #f])])
		    (if (not op)
			(Egg.Parser.Return e errors tokens)
		        (let ([tokens (cdr tokens)])
			  (if (null? tokens)
			      (Egg.Parser.Return.returnUnexpectedEOF tokens)
			      (let* ([parseRhs (Egg.Parser.parseBinop3 tokens)]
				     [rhs (Egg.Parser.Return.getResult parseRhs)]
				     [tokens (Egg.Parser.Return.getTokens parseRhs)])
				(if (not rhs)
				    (Egg.Parser.Return.addError `("Could not parse rhs") parseRhs)
				    (loop (Egg.Expr.Binop op e rhs)
					  (append (Egg.Parser.Return.getErrors parseRhs) errors)
					  tokens)))))))))))))

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
      (Egg.Parser.Return.returnUnexpectedEOF tokens)
      (let* ([parseLhs (Egg.Parser.parseExpr4 tokens)]
	     [lhs (Egg.Parser.Return.getResult parseLhs)]
	     [tokens (Egg.Parser.Return.getTokens parseLhs)])
	(if (not lhs)
	    (Egg.Parser.Return.addError `("Could not parse lhs") parseLhs)
	    (let* ([parseRhs (Egg.Parser.parseExpr4 tokens)]
		   [rhs (Egg.Parser.Return.getResult parseRhs)]
		   [tokens (Egg.Parser.Return.getTokens parseRhs)])
	      (if (not rhs)
		  (Egg.Parser.Return.addError `("Could not parse rhs") parseRhs)
		  (let loop ([e (Egg.Expr.App lhs rhs)]
			     [errors (append (Egg.Parser.Return.getErrors parseRhs)
					     (Egg.Parser.Return.getErrors parseLhs))]
			     [tokens tokens])
		    (let* ([parseRhs (Egg.Parser.parseExpr4 tokens)]
			   [rhs (Egg.Parser.Return.getResult parseRhs)]
			   [tokens (Egg.Parser.Return.getTokens parseRhs)])
		      (if (not rhs)
			  (Egg.Parser.Return e errors tokens)
			  (loop (Egg.Expr.App e rhs)
				(append (Egg.Parser.Return.getErrors parseRhs) errors)
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
  (if (null? tokens)
      (Egg.Parser.Return.returnUnexpectedEOF tokens)
      (if (not (Egg.Token.Id? (car tokens)))
	  (Egg.Parser.Return.returnUnexpectedToken 'Egg.Token.Id
						   tokens)
	  (Egg.Parser.Return (Egg.Expr.Var (Egg.Token.Id.getName (car tokens)))
			     '()
			     (cdr tokens)))))


(define (Egg.Parser.parseInt tokens)
  (if (null? tokens)
      (Egg.Parser.Return.returnUnexpectedEOF tokens)
      (if (not (Egg.Token.Int? (car tokens)))
	  (Egg.Parser.Return.returnUnexpectedToken 'Egg.Token.Int
						   tokens)
	  (Egg.Parser.Return (Egg.Expr.Int (Egg.Token.Int.getValue (car tokens)))
			     '()
			     (cdr tokens)))))


(define (Egg.Parser.parseExpr5 tokens)
  (Egg.Parser.parseParens tokens))
  

(define (Egg.Parser.parseParens tokens)
  (if (null? tokens)
      (Egg.Parser.Return.returnUnexpectedEOF tokens)
      (let* ([lp (car tokens)])
	(if (not (Egg.Token.LP? lp))
	    (Egg.Parser.Return.returnUnexpectedToken (Egg.Token.LP)
						     tokens) ;; Missing (
	    (let* ([tokens (cdr tokens)]
		   [parseExpr (Egg.Parser.parseExpr1 tokens)]
		   [expr (Egg.Parser.Return.getResult parseExpr)]
		   [tokens (Egg.Parser.Return.getTokens parseExpr)])
	      (if (null? tokens)
		  (Egg.Parser.Return.returnUnexpectedEOF tokens) ;; Missing )
		  (let* ([rp (car tokens)]
			 [tokens (cdr tokens)])
		    (if (not (Egg.Token.RP? rp))
			(Egg.Parser.Return.returnUnexpectedToken (Egg.Token.RP)
								 tokens) ;; Missing ) (or maybe unit expr)
			(Egg.Parser.Return expr tokens)))))))))
