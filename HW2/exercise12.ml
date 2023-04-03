let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> (f hd) || (fold f tl a)

let lany l p = fold p l false;;