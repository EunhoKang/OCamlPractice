let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> (f hd) || (fold f tl a)

let lany l p = fold p l false;;

(*
	 
let rec lany l p = 
	match l with 
	| [] -> false
	| hd :: tl -> (p hd) || (lany tl p) 
	
*)