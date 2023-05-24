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
	else 
		raise NotImplemented (* TODO *)

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
			if n1 <= n2 then (Bool false, m2) else (Bool true, m2)
		|_ -> raise UndefinedSemantics) 
	|EQ (e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let (v2, m2) = eval e2 env m1 in
		(match v1, v2 with
		|Int n1, Int n2 -> 
			if n1 = n2 then (Bool true, m2) else (Bool false, m2)
		|_ -> raise UndefinedSemantics)  
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
		let (v1, m1) = eval e1 env mem in
		(match v1 with
		| Bool true -> 
		let (v2, m2) = eval e2 env m1 in
		(eval (WHILE (e1, e2)) env m2)
		| Bool false -> (Unit, m1) 
		| _ -> raise UndefinedSemantics)
	|LET (v, e1, e2) ->
		let (v1, m1) = eval e1 env mem in
		let l = new_location() in
		let newe = extend_env (v, l) env in
		let (v, m) = eval e2 newe (extend_mem (l, v1) m1) in 
		(v, m)
	|PROC (vl, e) -> (Procedure (vl, e, env), mem)
	|CALLV (e, el) -> (*here*)
		let (list, m) = eval e env mem in
		match el with
		| [] -> (eval e env mem)
		| hd::tl -> 
			let (v1, m1) = eval hd env mem in
			let l = new_location() in
			(eval (CALLV (e, tl)) (extend_env (hd, l) env) (extend_mem (l, v1) m1))
	|CALLR (e, vl) ->
		let (v, m) = eval e env mem in
		(match vl with
		| [] -> (eval e env mem)
		| hd::tl -> 
			let y1 = apply_env env hd in
			(eval (CALLR (e, tl)) (extend_env (hd,y1) env) m))
	|ASSIGN (v, e) ->
		let l = new_location () in
		let (v, m) = eval e env mem in 
		(Loc (l), (extend_mem (l, v) m))
	|RECORD (vel) ->
		match vel with
		| [] -> (Record (), mem)
		| hd::tl -> 
			let l = new_location () in
			let (v1, m1) = eval hd env mem in 
			( RECORD (tl), (extend_mem (l, v1) m1))
	|FIELD (e, v) ->
		let (r, m1) = eval e env mem in
		let l = apply_env env r in 
		(apply_mem m1 l, m1)
	|ASSIGNF (e1, v ,e2) ->
		let (r, m1) = eval e1 env mem in
		let (v1, m2) = eval e2 env m1 in
		let l = apply_env v r in 
		(v1, (extend_mem (apply_mem m2 l, v1) m2))
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
	
	
