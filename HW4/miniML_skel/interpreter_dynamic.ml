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
	|CONST n -> Int n
  |VAR x -> apply_env env x
  |ADD (e1, e2) ->
    let v1 = eval e1 env mem in
    let v2 = eval e2 env mem in
    (match v1, v2 with
    |Int n1, Int n2 -> Int(n1 + n2)
    |_ -> raise (Failure "Type Error: non-numeric values"))
  |SUB (e1, e2) ->
    let v1 = eval e1 env mem in
    let v2 = eval e2 env mem in
    (match v1, v2 with
    |Int n1, Int n2 -> Int (n1 - n2)
    |_ -> raise (Failure "Type Error: non-numeric values")) 
  |MUL (e1, e2) ->
    let v1 = eval e1 env mem in
    let v2 = eval e2 env mem in
    (match v1, v2 with
    |Int n1, Int n2 -> Int (n1 * n2)
    |_ -> raise (Failure "Type Error: non-numeric values")) 
  |DIV (e1, e2) ->
    let v1 = eval e1 env mem in
    let v2 = eval e2 env mem in
    (match v1, v2 with
    |Int n1, 0 -> raise (Failure "Error: division-by-zero")) 
    |Int n1, Int n2 -> Int (n1 * n2)
    |_ -> raise (Failure "Type Error: non-numeric values")) 
  |EQ (e1, e2) ->
    let v1 = eval e1 env mem in
    let v2 = eval e2 env mem in
    (match v1, v2 with
    |Int n1, Int n2 -> if n1 = n2 then Bool true else false
    |_ -> raise (Failure "Type Error: non-numeric values"))   
  |LT (e1, e2) ->
    let v1 = eval e1 env mem in
    let v2 = eval e2 env mem in
    (match v1, v2 with
    |Int n1, Int n2 -> if n1 = n2 then Bool false else true
    |_ -> raise (Failure "Type Error: non-numeric values")) 
  |ISZERO e -> 
    (match eval e env mem with
    | Int 0 -> Bool true
    | Int _ -> Bool false
    | _ -> raise (Failure "Type Error: argument of iszero must be Boolean"))
  |READ -> Int (read_int ())
  |IF (e1,e2,e3) ->
    (match eval e1 env mem with 
    | Bool true -> eval e2 env mem 
    | Bool false -> eval e3 env mem 
    | _ -> raise (Failure "Type Error: condition must be Boolean"))
  |LET (x,e1,e2) ->
    let v1 = eval e1 env mem in
    let v = eval (extend_env (x,v1) env) e2 in v
  |LETREC (x1,x2,e1,e2) ->
    let v1 = eval e1 env mem in
    let v = eval (extend_env (x,v1) env) e2 in v    
  |LETMREC (x1,x2,e1,e2) ->
    let v1 = eval e1 env mem in
    let v = eval (extend_env (x,v1) env) e2 in v  
  |PROC (x,e) ->
    let v1 = eval e1 env mem in

    (* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
