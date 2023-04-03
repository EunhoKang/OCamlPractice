let rec gcd n m =
  match m with
  | 0 -> n
  | _ -> gcd m (n mod m);;