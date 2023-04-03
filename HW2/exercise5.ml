let rec perm n k =
  match n, k with
  | n, 0 -> 1
  | 0, k -> 1
  | _ -> n * (perm (n - 1) (k - 1));;