open Lang (* enable to use all stuff in lang.ml *)

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e) -> "Procedure "

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

(* represent the given environment as a string (use this for debugging) *)
let rec string_of_env env = 
	(List.fold_left (fun acc (x,v) -> Printf.sprintf "%s, %s |-> %s" acc x (value2str v)) "{" env) ^ "}" 

(* represent the given memory as a string (use this for debugging) *)
let rec string_of_mem mem = 
	(List.fold_left (fun acc (l,v) -> Printf.sprintf "%s, %d |-> %s" acc l (value2str v)) "{" mem) ^ "}" 
		 

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem ->
  match exp with 
	|CONST n -> (Int n, mem)
  |VAR x -> (apply_env env x, mem)
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
    |Int n1, Int n2 -> (Int (n1 * n2), m2)
    |_ -> raise UndefinedSemantics) 
  |EQ (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in
    (match v1, v2 with
    |Int n1, Int n2 -> 
      if n1 = n2 then (Bool true, m2) else (Bool false, m2)
    |_ -> raise UndefinedSemantics)   
  |LT (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in
    (match v1, v2 with
    |Int n1, Int n2 -> 
      if n1 = n2 then (Bool false, m2) else (Bool true, m2)
    |_ -> raise UndefinedSemantics) 
  |ISZERO e -> 
    let (v, m) = eval e env mem in
    (match v with
    | Int 0 -> (Bool true, m)
    | Int _ -> (Bool false, m)
    | _ -> raise UndefinedSemantics)
  |READ -> (Int (read_int ()), mem)
  |IF (e1, e2, e3) ->
    let (v1, m1) = eval e1 env mem in
    (match v1 with 
    | Bool true -> eval e2 env m1
    | Bool false -> eval e3 env m1 
    | _ -> raise UndefinedSemantics)
  |LET (x, e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let env1 = extend_env (x, v1) env in
    let (v, m) = eval e2 env1 m1 in (v, m)
  |LETREC (f, x, e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let env1 = extend_env (f, Procedure(x, e1)) env in
    let env2 = extend_env (x, v1) env1 in
    let (v, m) = eval e2 env2 m1 in (v, m)
  |LETMREC (f, x, ef, g, y, eg, e) ->
    let (vf, mf) = eval ef env mem in
    let envf = extend_env (f, Procedure(x, ef)) env in
    let (vg, mg) = eval eg envf mf in
    let env1 = extend_env (g, Procedure(y, eg)) envf in
    let (v, m) = eval e env1 mg in (v, m)
  |PROC (x, e) -> (Procedure (x, e), mem)
  |CALL (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v, m) = eval e2 env mem in
    (match v1 with
    |Procedure (x, e) -> 
      (eval e (extend_env (x, v) env) mem)
    |_ -> raise UndefinedSemantics)
  |NEWREF (e) ->
    let l = new_location () in
    let (v, m) = eval e env mem in 
    (v, (extend_mem (l, v) m))
  |DEREF (e) ->
    let (l, m) = eval e env mem in
    (match l with
    |Loc (l) -> (apply_mem m l, m)
    |_ -> raise UndefinedSemantics)
  |SETREF (e1, e2) ->
    let (l, m1) = eval e1 env mem in
    (match l with
    |Int l ->
      let (v, m2) = eval e2 env m1 in
      (v,(extend_mem (Loc l, v) m2))
    |_ -> raise UndefinedSemantics)
  |SEQ (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in (v2, m2)
  |BEGIN (e) ->
    let (v, m) = eval e env mem in (v, m)
    (* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
