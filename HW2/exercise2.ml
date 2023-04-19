let rec gcd n m =
  match m with
  | 0 -> n
  | _ -> gcd m (n mod m);;

  (*
  let rec gcd n m =
    let n, m = if n >= m them (n, m) else (m, n) in
      if m = 0 then n
      else gcd (n - m) m   
  *)