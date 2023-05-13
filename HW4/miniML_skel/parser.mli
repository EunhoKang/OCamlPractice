type token =
  | NUM of (int)
  | TRUE
  | FALSE
  | ID of (string)
  | ASSIGN
  | INT
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQUAL
  | EQUALEQUAL
  | LE
  | LT
  | GE
  | GT
  | NOT
  | AND
  | OR
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | READ
  | PRINT
  | SEMICOLON
  | LET
  | IN
  | COMMA
  | ISZERO
  | LETREC
  | PROC
  | NEWREF
  | DEREF
  | SETREF
  | BEGIN
  | END
  | LBRACE
  | RBRACE
  | LBLOCK
  | RBLOCK
  | LPAREN
  | RPAREN
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lang.program
