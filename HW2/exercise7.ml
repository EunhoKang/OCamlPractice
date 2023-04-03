type btree = Leaf | Node of int * btree * btree

let rec sum_tree bt =
  match bt with
  | Leaf -> 0
  | Node (i, b1, b2) -> i + (sum_tree b1) + (sum_tree b2);;