type 'a ntree = Leaf of 'a | Node of ('a ntree list)

let rec mapf f l =
  match l with
  | [] -> []
  | hd::tl -> (mapf f tl)@(f hd)

let rec length l =
  match l with
  | [] -> 0
  | hd::tl -> 
    match hd with
    | Leaf(a) -> 1 + length tl
    | Node(list) -> length tl

let rec max li =
  match li with
  | [] -> 0
  | hd :: tl -> if hd > (max tl) then hd else (max tl)

let rec find nt =
  match nt with
  | Leaf (a)-> []
  | Node (list) -> (mapf find list)@[length list]

let rec findn nt =
  max (find nt);;
