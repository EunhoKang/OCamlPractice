let rec nth l n =
	match l with
	| [] -> raise (Failure "list is too short")
	| hd::tl -> if n = 0 then hd else nth tl (n - 1)

let rec makeSubset l ol n =
  match n with 
  | -1 -> [l]
  | _ -> (makeSubset l ol (n - 1))@(makeSubset ((nth ol n)::l) ol (n - 1)) 

let rec length l =
  match l with
  | [] -> 0
  | hd::tl -> 1 + length tl

let rec powerset l = makeSubset [] l ((length l)-1);;

(*
let rec powerset l = 
	match l with 
	| [] -> [[]]
	| hd :: tl -> 
		let powerset_tl = powerset tl in 
		(map (fun s -> hd :: s) powerset_tl) @ powerset_tl  
*)