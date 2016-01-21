#lang racket
;; Type ::= int | [int]int | bool
;; Program ::= (var Id : Type; )∗ Function∗ Procedure+
;; Function ::= function Id((Id : Type, )∗) Returns;
;; Procedure ::= procedure Id((Id : Type, )∗) Returns?
;; Spec∗ {Body}
;; Spec ::= requires Expr; | ensures Expr;
;; | modifies (Id, )∗;
;; Returns ::= returns ((Id : Type, )∗)
;; Body ::= (var Id : Type; )∗ Stmt
;; Stmt ::= Id := Expr | Id[Expr] := Expr
;; | if (Expr) Stmt else Stmt
;; | Stmt; Stmt | havoc Id
;; | call (Id, )∗ := Id((Id, )∗) | return
;; | assume Expr | assert Expr

;; Body ::= VarDecls | Stmt+
;; Stmt ::= AssertStmt | AssumeStmt | AssignStmt | HavocStmt | IfStmt
;; AssignStmt ::= Id ':=' Expr ';'
;; AssertStmt ::= 'assert' Expr ';'
;; AssumeStmt ::= 'assume' Expr ';'
;; HavocStmt  ::= 'havoc' Id ';'
;; IfStmt     ::= 'if' Expr '{' Stmt+ '}' ('else' '{' Stmt+ '}')?
;; 
;; Expr ::=  
;; tokens:
;; (ID name)
;; (NUM value)
;; (KEYWORD symbol)
;; (PUNCT text)

(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/lex)

(define-lex-abbrev nonzerodigit (char-range #\1 #\9))
(define-lex-abbrev digit (char-range #\0 #\9))
(define-lex-abbrev decimalinteger (:or (:: nonzerodigit (:* digit)) (:+ #\0)))

(define boogie-lexer
  (lexer
   [(:or "assume" "assert" "if" "else" "while" "havoc")
    ; => 
    (cons `(KEYWORD, (string->symbol lexeme))
          (boogie-lexer input-port))]
   ["true"
    ; =>
    (cons `(LIT #t)
          (boogie-lexer input-port))]
   ["false"
    ; =>
    (cons `(LIT #f)
          (boogie-lexer input-port))]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    ; =>
    (cons `(ID ,(string->symbol lexeme))
          (boogie-lexer input-port))]
   [decimalinteger
    ; =>
    (cons `(LIT, (string->number lexeme))
          (boogie-lexer input-port))]
   [(:or "==" ":=" "!=")
    ; =>
    (cons `(PUNCT, lexeme)
          (boogie-lexer input-port))]
   [(:or #\( #\) #\+ #\* #\- #\; #\{ #\})
    ; ==>
    (cons `(PUNCT, lexeme)
          (boogie-lexer input-port))]
   
   [whitespace 
    ; =>
    (boogie-lexer input-port)]

   [(eof)
    '()]))


(boogie-lexer (open-input-string "if (x == y) {z := 1;} else {havoc x;}"))
