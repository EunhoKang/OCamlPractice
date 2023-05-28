type program = exp
and exp =
	| SKIP
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
	| LE of exp * exp
	| EQ of exp * exp
	| NOT of exp 
  | IF of exp * exp * exp
	| WHILE of exp * exp 
	| LET of var * exp * exp
	| PROC of var list * exp 
	| CALLV of exp * exp list 
	| CALLR of exp * var list
	| ASSIGN of var * exp 
	| RECORD of (var * exp) list 
	| FIELD of exp * var
	| ASSIGNF of exp * var * exp 
  | READ of var
	| PRINT of exp 
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int
  | Bool of bool
	| Unit
  | Procedure of var list * exp * env
	| Record of record
  | Loc of loc
and loc = int 
and env = (var * loc) list
and mem = (loc * value) list
and record = (var * loc) list

(* conversion of value to string *)
let value2str v =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "."  
	| Procedure (params,e,env) -> "Procedure "
  | Record record -> "Record "
	| Loc l -> "Loc "^(string_of_int l)

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::(List.filter (fun (l',_) -> l != l') m)
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int (l) ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

let counter = ref 0
let new_location () = counter:=!counter+1; (!counter)

(* conversion of env to string *)
let string_of_env env = 
	List.fold_left (fun str (x,l) -> Printf.sprintf "%s\n%s -> %d" str x l) "" env  
(* conversion of mem to string *)
let string_of_mem mem = 
	List.fold_left (fun str (l,v) -> Printf.sprintf "%s\n%d -> %s" str l (value2str v)) "" mem 		
		
exception NotImplemented
exception UndefinedSemantics
(* if the following variable is set true, gc will work (otherwise, gc simply returns a given memory). *)
let remove_garbage = ref false

let gc: env * mem -> mem
= fun (env, mem) ->
	if (not !remove_garbage) then mem 
	else raise NotImplemented
		

let rec eval : program -> env -> mem -> (value * mem)
=fun pgm env mem ->  
  match pgm with
  | READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)
	| PRINT e ->
		let v, mem' = eval e env mem in
		let _ = print_endline (value2str v) in
		(v, gc(env,mem')) (* Do not modify *) 
	|SKIP -> (Unit, mem)
	|TRUE -> (Bool true, mem)
	|FALSE -> (Bool false, mem)
	|CONST n -> (Int n, mem)
	|VAR x -> (apply_mem mem (apply_env env x), mem)
	|ADD (e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let (v2, m2) = eval e2 env m1 in
		(match v1, v2 with
		|Int n1, Int n2 -> (Int(n1 + n2), m2)
		|_ -> raise UndefinedSemantics)
	|SUB (e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let (v2, m2) = eval e2 env m1 in
		(match v1, v2 with
		|Int n1, Int n2 -> (Int (n1 - n2), m2)
		|_ -> raise UndefinedSemantics) 
	|MUL (e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let (v2, m2) = eval e2 env m1 in
		(match v1, v2 with
		|Int n1, Int n2 -> (Int (n1 * n2), m2)
		|_ -> raise UndefinedSemantics) 
	|DIV (e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let (v2, m2) = eval e2 env m1 in
		(match v1, v2 with
		|Int n1, Int 0 -> raise UndefinedSemantics 
		|Int n1, Int n2 -> (Int (n1 / n2), m2)
		|_ -> raise UndefinedSemantics)  
	|LE (e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let (v2, m2) = eval e2 env m1 in
		(match v1, v2 with
		|Int n1, Int n2 -> 
			if n1 <= n2 then (Bool true, m2) else (Bool false, m2)
		|_ -> raise UndefinedSemantics)  
	|EQ (e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let (v2, m2) = eval e2 env m1 in
		(match v1, v2 with
		|Int n1, Int n2 -> 
			if n1 = n2 then (Bool true, m2) else (Bool false, m2)
		|Bool b1, Bool b2 -> 
			if b1 = b2 then (Bool true, m2) else (Bool false, m2)
		|Unit, Unit -> 
			(Bool true, m2)
		|_ -> (Bool false, m2))  
	|NOT (e) -> 
		let (v, m) = eval e env mem in
		(match v with
		| Bool true -> (Bool false, m)
		| Bool false -> (Bool true, m)
		| _ -> raise UndefinedSemantics)
	|IF (e1, e2, e3) ->
		let (v1, m1) = eval e1 env mem in
		(match v1 with 
		| Bool true -> eval e2 env m1
		| Bool false -> eval e3 env m1 
		| _ -> raise UndefinedSemantics)
	|WHILE (e1, e2) -> 
		let (tf, m0) = eval e1 env mem in
		(match tf with
		| Bool true -> 
		let (v1, m1) = eval e2 env m0 in
		let (v2, m2) = eval (WHILE (e1, e2)) env m1 in
		(v2, m2)
		| Bool false -> (Unit, m0) 
		| _ -> raise UndefinedSemantics)
	|LET (x, e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let l = new_location() in
		let new_env = extend_env (x, l) env in
		let new_mem = extend_mem (l, v1) m1 in
		let (v, m) = eval e2 new_env new_mem in 
		(v, m)
	|PROC (vl, e) -> (Procedure (vl, e, env), mem)
	|CALLV (e0, el) ->
		let (proc, pm) = eval e0 env mem in
		(match proc with
		|Procedure (xl, e, ev) ->
			match (xl, el) with
			|(x::xtl, e1::etl) ->
				let (v, m) = eval e1 env pm in
				let l = new_location () in
				let new_env = (extend_env (x, l) ev) in
				let new_mem = (extend_mem (l, v) pm) in
				(eval (CALLV ((PROC (xtl, e)), etl)) new_env new_mem)
			|([], []) -> (eval e ev pm)
			|_ -> raise UndefinedSemantics
		|_ -> raise UndefinedSemantics)
	|CALLR (e0, vl) ->
		let (proc, pm) = eval e0 env mem in
		(match proc with
		|Procedure (xl, e, ev) ->
			match (xl, vl) with
			|(x::xtl, v::vtl) ->
				let new_env = (extend_env (x, (apply_env env v)) ev) in
				(eval (CALLR ((PROC (xtl, e)), vtl)) new_env pm)
			|([], []) -> eval e ev pm
			|_ -> raise UndefinedSemantics
		|_ -> raise UndefinedSemantics)
	|ASSIGN (x, e) ->
		let l = new_location () in
		let (v, m) = eval e env mem in 
		let new_loc = apply_env env x in
		let new_mem = (extend_mem (new_loc, v) m) in
		(v, (extend_mem (l, v) new_mem))
	|RECORD (vel) ->
		(match vel with
		|(x,e)::tl -> 
			let l = new_location () in
			let (v, m) = eval e env mem in
			let new_env = (extend_env (x, l) env) in
			(eval (RECORD (tl)) new_env (extend_mem (l, v) m))
		|[] -> 
			match env with
			|hd::tl -> (Record (env), mem)
			|_ -> (Unit, mem)
		|_ -> raise UndefinedSemantics)
	|FIELD (e, x) ->
		(let (r, m1) = eval e env mem in
		match r with
		|Record (list) ->
			let l = apply_env list x in 
			(apply_mem m1 l, m1)
		|_ -> raise UndefinedSemantics)
	|ASSIGNF (e1, x ,e2) ->
		(let (r, m1) = eval e1 env mem in
		match r with
		|Record (list) ->
			let (v, m2) = eval e2 env m1 in
			let rx = apply_env list x in 
			let new_mem = (extend_mem (rx, v) m2) in
			(v, new_mem)
		|_ -> raise UndefinedSemantics)
	|SEQ (e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let (v2, m2) = eval e2 env m1 in (v2, m2)
	|BEGIN (e) ->
		let (v, m) = eval e env mem in (v, m)


let run : program -> bool -> bool -> unit 
= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
		print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))
	
	
