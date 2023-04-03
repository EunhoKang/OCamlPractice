let rec power x n =
  match n with
  | 0 -> 1
  | _ -> x * (power x (n - 1));;