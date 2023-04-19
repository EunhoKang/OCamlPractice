let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> (f hd) && (fold f tl a)

let lall l p = fold p l true;;

(*
let rec lall l p = 
	match l with 
	| [] -> true
	| hd :: tl -> (p hd) && (lall tl p) 
*)