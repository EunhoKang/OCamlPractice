let rec cartesian l1 l2 =
  match l1, l2 with
  | [], l2 -> []
  | l1, [] -> []
  | h1::t1, h2::t2 -> [(h1, h2)]@(cartesian [h1] t2)@(cartesian t1 l2);;