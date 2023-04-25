let rec sumRoutine l n=
  match l with
  | [] -> 0
  | hd::tl -> 
    if n = 1 then (sumRoutine tl 0) + hd
    else (sumRoutine tl 1) - hd

let rec alterSum l =
  match l with
  | [] -> 0
  | hd::tl -> hd + (sumRoutine tl 1);;