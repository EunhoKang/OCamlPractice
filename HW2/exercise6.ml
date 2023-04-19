let rec cartesian l1 l2 =
  match l1, l2 with
  | [], l2 -> []
  | l1, [] -> []
  | h1::t1, h2::t2 -> [(h1, h2)]@(cartesian [h1] t2)@(cartesian t1 l2);;

  (*
  let rec map f l =
    match l with
    | [] -> []
    |hd :: tl -> (f hd) :: (map f tl)
  
  let rec cartesian l1 l2 =
    match l1 with
    | [] -> []
    | hd1 :: tl1 ->
      (map (fun x -> (hd1, x)) l2) @ (cartesian tl1 l2)
  *)