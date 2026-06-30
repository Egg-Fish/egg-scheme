;;; Chez Scheme R5RS Compliance
;;; ---------------------------
;;; Execute this snippet to force R5RS compliance in a
;;; Chez Scheme REPL.

(interaction-environment
  (copy-environment
    (environment
      '(rnrs)
      '(rnrs eval) 
      '(rnrs mutable-pairs) 
      '(rnrs mutable-strings) 
      '(rnrs r5rs)
      '(only (chezscheme) import library load sort trace untrace))
    #t))
