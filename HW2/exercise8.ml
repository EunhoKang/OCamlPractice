type btree = Leaf | Node of int * btree * btree

let rec prod_tree bt =
  match bt with
  | Leaf -> 1
  | Node (i, b1, b2) -> i * (prod_tree b1) * (prod_tree b2);;