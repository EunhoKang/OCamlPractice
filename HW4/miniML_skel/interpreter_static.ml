open Lang (* enable to use all stuff in lang.ml *)

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
	| MRecProcedure of var * var * exp * var * var * exp * env 
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
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f
	| MRecProcedure (f,x,e1,g,y,e2,env) -> "MRecProcedure " ^ f ^ " and " ^ g 

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
    |Int n1, Int n2 -> (Int (n1 / n2), m2)
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
    let env1 = extend_env (f, RecProcedure(f, x, e1, env)) env in
    let (v, m) = eval e2 env1 mem in (v, m)
  |LETMREC (f, x, ef, g, y, eg, e) ->
    let envf = extend_env (f, MRecProcedure(f, x, ef, g, y, eg, env)) env in
    let envg = extend_env (g, MRecProcedure (f, x, ef, g, y, eg, env)) envf in
    let (v, m) = eval e envg mem in (v, m) 
  |PROC (x, e) -> (Procedure (x, e, env), mem)
  |CALL (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v, m) = eval e2 env mem in
    (match v1 with
    |Procedure (x, e, ev) -> 
      (eval e (extend_env (x, v) ev) mem)
    |RecProcedure (f, x, e, ev) ->
      (eval e (extend_env (x, v) (extend_env (f, RecProcedure (f, x, e, ev)) ev)) mem)
    |MRecProcedure (f, x, ef, g, y, eg, ev) ->
      (let envf = extend_env (f, MRecProcedure (f, x, ef, g, y, eg, ev)) ev in
      let env1 = extend_env (g, MRecProcedure (f, x, ef, g, y, eg, ev)) envf in
      (eval eg (extend_env (x, v) env1) mem))
    |_ -> raise UndefinedSemantics)
  |NEWREF (e) ->
    let l = new_location () in
    let (v, m) = eval e env mem in 
    (Loc (l), (extend_mem (l, v) m))
  |DEREF (e) ->
    let (l, m) = eval e env mem in
    (match l with
    |Loc (l) -> (apply_mem m l, m)
    |_ -> raise UndefinedSemantics)
  |SETREF (e1, e2) ->
    let (l, m1) = eval e1 env mem in
    let (v, m2) = eval e2 env m1 in
    (match l with
    |Loc (loc) ->
      (v, (extend_mem (loc, v) m2))
    |_ -> raise UndefinedSemantics)
  |SEQ (e1, e2) ->
    let (v1, m1) = eval e1 env mem in
    let (v2, m2) = eval e2 env m1 in (v2, m2)
  |BEGIN (e) ->
    let (v, m) = eval e env mem in (v, m)
  
(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
