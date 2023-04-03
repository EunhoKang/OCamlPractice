let rec comb n k =
  match n, k with
  | n, 0 -> 1
  | 0, k -> 1
  | _ -> if n = k then 1 else (comb (n - 1) k) + (comb (n - 1) (k - 1));;
