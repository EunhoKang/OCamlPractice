type 'a ntree = Leaf of 'a | Node of ('a ntree list)

let rec mapf f l =
  match l with
  | [] -> 0
  | hd::tl -> (mapf f tl) + (f hd)

let rec findn nt =
  match nt with
  | Leaf (a)-> 1
  | Node (list) -> 
    let cnt = (mapf findn list)
    if ;;
