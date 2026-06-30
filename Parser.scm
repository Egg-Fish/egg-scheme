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
  (let ([parseLambda (Egg.Parser.parseLambda tokens)]
	[parseLet (Egg.Parser.parseLet tokens)])
    (cond
     [(and (pair? tokens)
	   (Egg.Token.Lambda? (car tokens)))
      parseLambda]

     [(and (pair? tokens)
	   (Egg.Token.Let? (car tokens)))
      parseLet]

     [else
      (Egg.Parser.parseExpr2 tokens)])))


(define (Egg.Parser.parseLambda tokens)
  (define (parseLambda tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let ([t (car tokens)])
	  (cond
	   [(Egg.Token.Lambda? t)
	    (Egg.Parser.Return (Egg.Expr.Var (Egg.Token.Lambda.getParameter t))
			       '()
			       (cdr tokens))]

	   [(Egg.Token.Id? t)
	    (Egg.Parser.Return (Egg.Expr.Var (Egg.Token.Id.getName t))
			       `(("Parsing id as lambda parameter" ,tokens))
			       (cdr tokens))]

	   [else
	    (Egg.Parser.Return.returnUnexpectedToken (Egg.Token.Let) tokens)]))))

  (define (parseDot tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let ([t (car tokens)])
	  (if (not (Egg.Token.Dot? t))
	      (Egg.Parser.Return (Egg.Expr.Unknown)
				 `(("Missing ., skipping" ,tokens))
				 tokens)
	      (Egg.Parser.Return (Egg.Expr.Unknown)
				 '()
				 (cdr tokens))))))

  (define (parseBody tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let* ([ret (Egg.Parser.parseExpr1 tokens)]
	       [body (Egg.Parser.Return.getResult ret)]
	       [tokens (Egg.Parser.Return.getTokens ret)])
	  (if (not body)
	      (Egg.Parser.Return.addError `("Could not parse body") ret)
	      ret))))

  (let* ([ret (parseLambda tokens)]
	 [param (Egg.Parser.Return.getResult ret)]
	 [errors (Egg.Parser.Return.getErrors ret)]
	 [tokens (Egg.Parser.Return.getTokens ret)])
    (if (not param)
	(Egg.Parser.Return.addError `("Missing parameter") ret)
	(let* ([ret (parseDot tokens)]
	       [lhs (Egg.Parser.Return.getResult ret)]
	       [errors (append (Egg.Parser.Return.getErrors ret) errors)]
	       [tokens (Egg.Parser.Return.getTokens ret)])
	  (if (not lhs)
	      ret
	      (let* ([ret (parseBody tokens)]
		     [body (Egg.Parser.Return.getResult ret)]
		     [errors (append (Egg.Parser.Return.getErrors ret) errors)]
		     [tokens (Egg.Parser.Return.getTokens ret)])
		(if (not body)
		    ret
		    (Egg.Parser.Return (Egg.Expr.Lambda param body)
				       errors
				       tokens))))))))


(define (Egg.Parser.parseLet tokens)
  (define (parseLet tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let ([t (car tokens)])
	  (if (not (Egg.Token.Let? t))
	      (Egg.Parser.Return.returnUnexpectedToken (Egg.Token.Let) tokens)
	      (Egg.Parser.Return (Egg.Expr.Unknown)
				 '()
				 (cdr tokens))))))

  (define (parseLhs tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let* ([ret (Egg.Parser.parseVar tokens)]
	       [lhs (Egg.Parser.Return.getResult ret)]
	       [tokens (Egg.Parser.Return.getTokens ret)])
	  (if (not lhs)
	      (Egg.Parser.Return.addError `("Could not parse lhs") ret)
	      ret))))

  (define (parseEqual tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let ([t (car tokens)])
	  (if (not (Egg.Token.Equal? t))
	      (Egg.Parser.Return (Egg.Expr.Unknown)
				 `(("Missing =, skipping" ,tokens))
				 tokens)
	      (Egg.Parser.Return (Egg.Expr.Unknown)
				 '()
				 (cdr tokens))))))

  (define (parseRhs tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let* ([ret (Egg.Parser.parseExpr1 tokens)]
	       [rhs (Egg.Parser.Return.getResult ret)]
	       [tokens (Egg.Parser.Return.getTokens ret)])
	  (if (not rhs)
	      (Egg.Parser.Return.addError `("Could not parse rhs") ret)
	      ret))))

  (define (parseIn tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let ([t (car tokens)])
	  (if (not (Egg.Token.In? t))
	      (Egg.Parser.Return (Egg.Expr.Unknown)
				 `(("Missing in, skipping" ,tokens))
				 tokens)
	      (Egg.Parser.Return (Egg.Expr.Unknown)
				 '()
				 (cdr tokens))))))

  (define (parseBody tokens)
    (if (null? tokens)
	(Egg.Parser.Return.returnUnexpectedEOF tokens)
	(let* ([ret (Egg.Parser.parseExpr1 tokens)]
	       [body (Egg.Parser.Return.getResult ret)]
	       [tokens (Egg.Parser.Return.getTokens ret)])
	  (if (not body)
	      (Egg.Parser.Return.addError `("Could not parse body") ret)
	      ret))))

  (let* ([ret (parseLet tokens)]
	 [res (Egg.Parser.Return.getResult ret)]
	 [errors (Egg.Parser.Return.getErrors ret)]
	 [tokens (Egg.Parser.Return.getTokens ret)])
    (if (not res)
	(Egg.Parser.Return.addError `("Missing let") ret)
	(let* ([ret (parseLhs tokens)]
	       [lhs (Egg.Parser.Return.getResult ret)]
	       [errors (append (Egg.Parser.Return.getErrors ret) errors)]
	       [tokens (Egg.Parser.Return.getTokens ret)])
	  (if (not lhs)
	      ret
	      (let* ([ret (parseEqual tokens)]
		     [res (Egg.Parser.Return.getResult ret)]
		     [errors (append (Egg.Parser.Return.getErrors ret) errors)]
		     [tokens (Egg.Parser.Return.getTokens ret)])
		(if (not res)
		    (Egg.Parser.Return.addError `("Missing =") ret)
		    (let* ([ret (parseRhs tokens)]
			   [rhs (Egg.Parser.Return.getResult ret)]
			   [errors (append (Egg.Parser.Return.getErrors ret) errors)]
			   [tokens (Egg.Parser.Return.getTokens ret)])
		      (if (not rhs)
			  ret
			  (let* ([ret (parseIn tokens)]
				 [res (Egg.Parser.Return.getResult ret)]
				 [errors (append (Egg.Parser.Return.getErrors ret) errors)]
				 [tokens (Egg.Parser.Return.getTokens ret)])
			    (if (not res)
				(Egg.Parser.Return.addError `("Missing in") ret)
				(let* ([ret (parseBody tokens)]
				       [body (Egg.Parser.Return.getResult ret)]
				       [errors (append (Egg.Parser.Return.getErrors ret) errors)]
				       [tokens (Egg.Parser.Return.getTokens ret)])
				  (if (not body)
				      ret
				      (Egg.Parser.Return (Egg.Expr.Let lhs rhs body)
							 errors
							 tokens))))))))))))))

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
		   [errors (Egg.Parser.Return.getErrors parseExpr)]
		   [tokens (Egg.Parser.Return.getTokens parseExpr)])
	      (if (null? tokens)
		  (Egg.Parser.Return.returnUnexpectedEOF tokens) ;; Missing )
		  (let* ([rp (car tokens)]
			 [tokens (cdr tokens)])
		    (if (not (Egg.Token.RP? rp))
			(Egg.Parser.Return.returnUnexpectedToken (Egg.Token.RP)
								 tokens) ;; Missing ) (or maybe unit expr)
			(Egg.Parser.Return expr errors tokens)))))))))
