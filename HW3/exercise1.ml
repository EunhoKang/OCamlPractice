let rec reverse l = 
	match l with
	| [] -> []
	| hd :: tl -> (reverse tl) @ [hd]

let rec revrev l = 
	match l with
  | [] -> []
  | [[]] -> [[]]
	| hd :: tl -> (revrev tl) @ [reverse hd];;