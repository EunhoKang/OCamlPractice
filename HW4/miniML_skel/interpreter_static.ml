open Lang (* enable to use all stuff in lang.ml *)

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
	| MRecProcedure of (* complete this definition *) 
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
=fun exp env mem -> raise NotImplemented
  
(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
