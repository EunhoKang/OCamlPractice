let rec reverse l = 
	match l with
	| [] -> []
	| hd :: tl -> (reverse tl)@[hd]

let rec rtake l n =
  match (reverse l) with
  | [] -> []
  | hd::tl -> if n = 0 then [] else (rtake (reverse tl) (n - 1))@[hd];;

  (*
let rec length l = 
	match l with [] -> 0 | _ :: tl -> 1 + (length tl)

let rec rtake l n =
  match l with
  | [] -> []
  | hd :: tl ->
    if n = 0 then []
    else if length tl < n then l
    else rtake tl n

  *)