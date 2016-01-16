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

(define (nnf expr)
  (match expr
    [`(¬ ,exp) (match exp
                 [`(Var ⊥) `(Var ⊤)]
                 [`(Var ⊤) `(Var ⊥)]
                 [`(Var ,var) `(¬ (Var ,var))]
                 [`(∧ ,left ,right) `(∨ (¬ ,(nnf left)) (¬ ,(nnf right)))]
                 [`(∨ ,left ,right) `(∧ (¬ ,(nnf left)) (¬ ,(nnf right)))]
                 [`(→ ,left ,right) `(∧ ,(nnf left) ,(nnf `(¬ ,right)))]
                 [`(↔ ,left ,right) `(¬ (∧ (→ ,left ,right) (→ ,right ,left)))]
                 [`(¬ ,e) e])]
    [`(,op ,left ,right) `(,op ,(nnf left) ,(nnf right))]
    [else expr]))

(define (nnf-fix expr)
  (walk-fix nnf expr))

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
    [`(¬ (Var ,x))
     ; => 
     (list expr)]
    [`(Var ,x)
     ; => 
     (list expr)]
    [`(∨ (∧ ,a ,b) (∧ ,c ,d))
     ; =>
     (list `(∨ ,a ,c) `(∨ ,a ,d) `(∨ ,b ,c) `(∨ ,b ,d))]
    [`(∨ (∧ ,a ,b) ,c)
     ;=>
     (list `(∨ ,a ,c) `(∨ ,b ,c))]
    [`(∨ ,a (∧ ,b ,c))
     ;=>
     (list `(∨ ,a ,b) `(∨ ,a ,c))]
    [`(∨ (→ ,a ,b) (→ ,c ,d))
     ; =>
     (list `(∨ (∨ (¬ ,a) ,b) (∨ (¬ ,c) ,d)))]
    [`(∨ (→ ,a ,b) ,c)
     ; =>
     (list `(∨ (∨ (¬ ,a) ,b) ,c))]
    [`(∨ ,a (→ ,b ,c))
     ; =>
     (list `(∨ ,a (∨ (¬ ,b) ,c)))]
    [`(∨ ,a ,b)
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
      [`(¬ (Var ,x)) (list expr empty)]
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

(define (cnf l-expr)
  (walk-fix (λ (x)
              (list->cnf
                   (map nnf-fix x)))
            l-expr))

(define (lex-this lexer input) (lambda () (lexer input)))

(define var-count 1)

(define (add-var v h)
  (if (hash-has-key? h v)
      void
      (begin
        (hash-set! h v var-count)
        (set! var-count (+ var-count 1)))))

(define (add-vars formula h)
  (match formula
    [`(¬ (Var ,x)) (begin
                     (add-var `(Var ,x) h)
                     h)]
    [`(Var ,x) (begin
                 (add-var `(Var ,x) h)
                 h)]
    [`(,op ,a ,b) (begin
                    (add-vars a h) (add-vars b h))]
    [`(,op ,a) (add-vars a h)]))

(define (gather-args! cnf-list (h (make-hash)))
  (cond
    [(empty? cnf-list) h]
    [else (begin
            (add-vars (first cnf-list) h)
            (gather-args! (rest cnf-list) h))]))

(define (dimacs-header expr h)
  (string-append "p cnf "
                 (number->string (apply max (hash-values h)))
                 " "
                 (number->string (length expr))))

(define (to-dimacs-lower expr h)
   (match expr
       [`(¬ (Var ,x)) 
        (string-append 
         " -"
         (number->string 
          (hash-ref! h `(Var ,x) "-1")))]
     [`(Var ,x)
      (string-append 
       " "
       (number->string
        (hash-ref! h `(Var ,x) "-1")))]
     [`(∨ ,a ,b) (string-append
                  (to-dimacs-lower a h)
                  (to-dimacs-lower b h))]))

(define (to-dimacs-line expr h)
  (string-append (to-dimacs-lower expr h) " 0"))

(define (to-dimacs expr h)
  (string-append
   (dimacs-header expr h) "\n"
   (apply string-append
          (map (λ (ex)
                 (string-append (to-dimacs-line ex h) "\n")) expr))))
        

(let ((input (open-input-file "test.expr")))
  (let [(result
        (cnf
         (tseitin-trans
          (eliminate-bicond 
           (eliminate-xor 
            (bool-parser 
             (lex-this bool-lexer input)))))))]
    (let [(args(gather-args! result))]
      (display (to-dimacs result args)))))
    
