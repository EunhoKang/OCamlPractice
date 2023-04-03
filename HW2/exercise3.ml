let rec max li =
  match li with
  | [] -> 0
  | hd :: tl -> if hd > (max tl) then hd else (max tl);;