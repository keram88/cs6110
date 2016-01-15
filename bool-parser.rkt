#lang racket
(require parser-tools/lex 
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens a (VAR))
(define-empty-tokens b (∨ ∧ → ← ¬ ↔ ⊤ ⊥ ⊕ OPAREN CPAREN EOF if then))

(define bool-lexer
  (lexer
   ("if" (token-if))
   ("then" (token-then))
   ((:or "|" "+" "or") (token-∨))
   ((:or "&" "*" "^" "and") (token-∧))
   ((:or ">" "->" "=>") (token-→))
   ((:or "<" "<-" "<=") (token-←))
   ((:or "!" "~" "-" "not") (token-¬))
   ((:or "T" "1") (token-⊤))
   ((:or "F" "C" "0") (token-⊥))
   ("(" (token-OPAREN))
   (")" (token-CPAREN))
   ((:or "<>" "=" "<->" "<=>" "iff") (token-↔))
   ((:or "</>" "!=" "xor") (token-⊕))
   (whitespace (bool-lexer input-port))
   ((:+ alphabetic) (token-VAR lexeme))
   ((eof) (token-EOF))))

(define bool-parser
  (parser
   (start bidirect-expr)
   (end EOF)
   (error void)
   (tokens a b)
   (precs (left ∨ ∧ → ↔ ⊕))
   (grammar
    (bidirect-expr 
     ((bidirect-expr ↔ impl-expr)
      `(↔ ,$1 ,$3))
     ((bidirect-expr ⊕ impl-expr)
      `(⊕ ,$1 ,$3))
     ((impl-expr)
      $1))
    
    (impl-expr 
     ((if impl-expr then or-expr)
      `(→ ,$2 ,$4))
     ((impl-expr → or-expr)
      `(→ ,$1 ,$3))
     ((impl-expr ← or-expr)
      `(→ ,$3 ,$1))
     ((or-expr)
      $1))
    (or-expr ((or-expr ∨ and-expr)
              `(∨ ,$1 ,$3))
             ((and-expr)
              $1))
    (and-expr ((and-expr ∧ not-expr)
               `(∧ ,$1 ,$3))
              ((not-expr)
               $1))
    (not-expr ((¬ not-expr)
               `(¬ ,$2))
              ((atom)
               $1))
    (atom ((VAR)
           `(Var ,(string->symbol $1)))
          ((⊤)
           `(Var ⊤))
          ((⊥)
           `(Var ⊥))
          ((OPAREN bidirect-expr CPAREN)
           $2))
    )))

(define (eliminate-xor expr)
  (match expr
    [`(⊕ ,a ,b)
     ;=>
     (let [(nxa (eliminate-xor a))
           (nxb (eliminate-xor b))]
       `(∨ (∧ (¬ ,nxa) ,nxb) (∧ ,nxa (¬ ,nxb))))]
    [`(,op ,a ,b)  `(,op ,(eliminate-xor a) ,(eliminate-xor b))]
    [`(Var ,x) expr]
    [`(,op ,a) `(,op ,(eliminate-xor a))]))

(define (eliminate-bicond expr)
  (match expr
    [`(↔ ,a ,b)
     ;=>
     (let [(nxa (eliminate-bicond a))
           (nxb (eliminate-bicond b))]
       `(∧ (→ ,nxa ,nxb) (→ ,nxb ,nxa)))]
    [`(,op ,a ,b)  `(,op ,(eliminate-bicond a) ,(eliminate-bicond b))]
    [`(Var ,x) expr]
    [`(,op ,a) `(,op ,(eliminate-bicond a))]))

(define (to-cnf expr) ; Returns a list of clauses
  (match expr
    [`(↔ ,a ,b)
     ; =>
     (list `(→ ,a ,b) `(→ ,b ,a))]
    [`(→ ,a ,b)
     ; =>
     (list `(∨ (¬ ,a) ,b))]
    [`(∨ ,a ,b)
     ; => 
     (list expr)]
    [`(Var ,x)
     ; => 
     (list expr)]))

(define (list->cnf l-expr)
  (append* (map to-cnf l-expr)))

(define (genvar)
  `(Var ,(gensym)))

(define (tseitin-trans expr)
  (let [(transformed (tseitin-real expr))]
    (cons (first transformed) (second transformed))))

(define (tseitin-real expr)
  (let [(sub-name (genvar))]
    (match expr
      [`(Var ,x) (list expr empty)]
      [`(,op ,a ,b)
       ; => 
       (let [(res-a (tseitin-real a))
             (res-b (tseitin-real b))]
         (list sub-name
               (cons `(↔ ,sub-name (,op ,(first res-a) ,(first res-b)))
                     (append (second res-a) (second res-b)))))]
      [`(,op ,a)       
       ; =>
       (let [(res-a (tseitin-real a))]
         (list sub-name
               (cons `(↔ ,sub-name (,op ,(first res-a)))
                     (second res-a))))])))

(define (walk-fix proc expr (old '()))
  (if (equal? old expr)
      expr
      (begin
        (walk-fix proc (proc expr) expr))))

(define (lex-this lexer input) (lambda () (lexer input)))

(let ((input (open-input-string "(a -> c) iff 1")))
  
  
  (display (list->cnf (list->cnf (tseitin-trans (eliminate-bicond (eliminate-xor (bool-parser (lex-this bool-lexer input)))))))))