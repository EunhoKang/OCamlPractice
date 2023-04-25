let rec insert li n =
  match li with
  | [] -> [n]
  | hd::tl -> 
    if n > hd then n::li
    else hd::(insert tl n)

let rec dsort li =
  match li with
  | [] -> []
  | hd::tl -> insert (dsort tl) hd;;