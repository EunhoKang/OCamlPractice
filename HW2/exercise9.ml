let rec ltake l n =
  match l with
  | [] -> []
  | hd::tl -> if n = 0 then [] else hd::(ltake tl (n - 1));;