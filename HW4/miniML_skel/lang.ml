exception NotImplemented
exception UndefinedSemantics

type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
	| EQ of exp * exp
	| LT of exp * exp  
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of var * var * exp * var * var * exp * exp 
  | PROC of var * exp
  | CALL of exp * exp
  | NEWREF of exp 
  | DEREF of exp
  | SETREF of exp * exp
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

(* represent the given program in the OCaml datatype *)				 
let rec string_of_exp exp = 
	match exp with 
	| CONST n -> Printf.sprintf "CONST (%d)" n
	| VAR x ->  Printf.sprintf "VAR (%s)" x
  | ADD (e1, e2) -> Printf.sprintf "ADD (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | SUB (e1, e2) -> Printf.sprintf "SUB (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | MUL (e1, e2) -> Printf.sprintf "MUL (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | DIV (e1, e2) -> Printf.sprintf "DIV (%s, %s)" (string_of_exp e1) (string_of_exp e2)
	| EQ (e1, e2) -> Printf.sprintf "EQ (%s, %s)" (string_of_exp e1) (string_of_exp e2)
	| LT (e1, e2) -> Printf.sprintf "LT (%s, %s)" (string_of_exp e1) (string_of_exp e2)  
  | ISZERO e -> Printf.sprintf "ISZERO (%s)" (string_of_exp e)
  | READ -> "READ"
  | IF (e1, e2, e3) -> 
		Printf.sprintf "IF (%s, %s, %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | LET (x, e1, e2) -> 
		Printf.sprintf "LET (%s, %s, %s)" x (string_of_exp e1) (string_of_exp e2)
  | LETREC (f, x, e1, e2) -> 
		Printf.sprintf "LETREC (%s, %s, %s, %s)" f x (string_of_exp e1) (string_of_exp e2)
  | LETMREC (f, x, e1, g, y, e2, e3) -> 
		Printf.sprintf "LETMREC (%s, %s, %s, %s, %s, %s, %s)" 
			f x (string_of_exp e1) g y (string_of_exp e2) (string_of_exp e3) 
  | PROC (x, e) -> Printf.sprintf "PROC (%s, %s)" x (string_of_exp e)		
  | CALL (e1, e2) -> Printf.sprintf "CALL (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | NEWREF e -> Printf.sprintf "NEWREF (%s)" (string_of_exp e) 
  | DEREF e -> Printf.sprintf "DEREF (%s)" (string_of_exp e)
  | SETREF (e1, e2) -> Printf.sprintf "SETREF (%s, %s)" (string_of_exp e1) (string_of_exp e2) 
  | SEQ (e1, e2) -> Printf.sprintf "SEQ (%s, %s)" (string_of_exp e1) (string_of_exp e2)
  | BEGIN e -> Printf.sprintf "BEGIN %s" (string_of_exp e) 
