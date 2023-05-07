%{

%}

%token <int> NUM
%token TRUE FALSE
%token <string> ID
%token ASSIGN INT PLUS MINUS STAR SLASH EQUAL EQUALEQUAL LE LT GE GT NOT AND OR IF THEN
ELSE WHILE DO READ PRINT SEMICOLON LET IN COMMA ISZERO LETREC PROC NEWREF DEREF
SETREF BEGIN END
%token LBRACE RBRACE LBLOCK RBLOCK LPAREN RPAREN EOF

%left SEMICOLON
%left OR
%left AND
%left LT LE GT GE EQUALEQUAL
%left PLUS MINUS
%left STAR SLASH 
%right NOT 


%start program
%type <Lang.program> program
%%

program:
    exp EOF { $1 }
    ;

exp:
    NUM { Lang.CONST $1 }
  | ID { Lang.VAR $1 }
  | exp PLUS exp  { Lang.ADD ($1,$3) }
  | exp MINUS exp  { Lang.SUB ($1,$3) }
  | exp STAR exp  { Lang.MUL ($1,$3) }
  | exp SLASH exp  { Lang.DIV ($1,$3) }
	| exp EQUAL exp  { Lang.EQ ($1,$3) }
	| exp LT exp  { Lang.LT ($1,$3) }  
  | ISZERO exp { Lang.ISZERO $2 }
  | IF exp THEN exp ELSE exp { Lang.IF ($2,$4,$6) }
  | LET ID EQUAL exp IN exp { Lang.LET ($2,$4,$6) }
  | LETREC ID LPAREN ID RPAREN EQUAL exp IN exp { Lang.LETREC ($2,$4,$7,$9) }
	| LETREC ID LPAREN ID RPAREN EQUAL exp AND ID LPAREN ID RPAREN EQUAL exp IN exp { 
			Lang.LETMREC ($2,$4,$7,$9,$11,$14,$16) 
		} 
  | PROC LPAREN ID RPAREN exp { Lang.PROC ($3,$5) } 
  | LPAREN exp exp RPAREN { Lang.CALL ($2,$3) }
  | NEWREF LPAREN exp RPAREN { Lang.NEWREF $3 }
  | DEREF LPAREN exp RPAREN { Lang.DEREF $3 }
  | exp ASSIGN exp  { Lang.SETREF ($1, $3) }
  | exp SEMICOLON exp  { Lang.SEQ ($1, $3) }
  | LPAREN exp RPAREN { $2 }
  | READ { Lang.READ }
  | BEGIN exp END { Lang.BEGIN $2 }
%%

let parse_error s = print_endline s
