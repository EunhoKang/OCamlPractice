let rec union l1 l2=
  match (List.sort compare l1), (List.sort compare l2) with
  | [], [] -> []
  | hd::tl, [] -> l1
  | [], hd::tl -> l2
  | h1::t1, h2::t2 -> 
    if h1 < h2 then h1::(union t1 l2)
    else if h1 > h2 then h2::(union l1 t2)
    else h1::(union t1 t2);;