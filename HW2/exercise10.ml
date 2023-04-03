let rec reverse l = 
	match l with
	| [] -> []
	| hd :: tl -> (reverse tl)@[hd]

let rec rtake l n =
  match (reverse l) with
  | [] -> []
  | hd::tl -> if n = 0 then [] else (rtake (reverse tl) (n - 1))@[hd];;