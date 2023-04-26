type 'a ntree = Leaf of 'a | Node of ('a ntree list)

let rec mapf f l =
  match l with
  | [] -> []
  | hd::tl -> (mapf f tl)@(f hd)

let rec flatten nt =
  match nt with
  | Leaf (a)-> [a]
  | Node (list) -> (mapf flatten list);;