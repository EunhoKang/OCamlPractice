let rec iter n f x= 
  if n > 0 then iter (n - 1) f (f x)
  else x;;

let rec mapn f n l =
  match l with
  | [] -> []
  | hd::tl -> (iter n f hd)::(mapn f n tl);;