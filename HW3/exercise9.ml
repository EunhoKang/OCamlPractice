type formula = TRUE | FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
and value = Int of int | Bool of bool


let rec calc : expr -> value
=fun ex ->
  match ex with
  | NUM (e) -> Int e
  | PLUS (e1, e2) -> 
    let i1 = calc e1 in
    let i2 = calc e2 in
    match i1, i2 with
    | Int i1, Int i2 -> Int (i1 + i2)
  | MINUS (e1, e2) -> 
    let i1 = calc e1 in
    let i2 = calc e2 in
    match i1, i2 with
    | Int i1, Int i2 -> Int (i1 - i2)

let rec eval : formula -> value =
  match fm with
  | Bool (TRUE) -> Bool (TRUE)
  | FALSE -> Bool (FALSE) 
  | NOT (f) -> 
    let v = eval v in
    match v with
    | Bool (TRUE) -> Bool (FALSE) 
    | Bool (FALSE)  -> Bool (TRUE)
  | ANDALSO (f1, f2) ->
    let v1 = eval f1 in
    let v2 = eval f2 in
    match v1, v2 with
    | Bool (TRUE), Bool (TRUE) -> Bool (TRUE)
    | _ -> Bool (FALSE) 
  | ORELSE (f1, f2) ->
    let v1 = eval f1 in
    let v2 = eval f2 in
    match v1, v2 with
    | Bool (FALSE) , Bool (FALSE)  -> Bool (FALSE) 
    | _ -> Bool (TRUE)
  | IMPLY (f1, f2) ->
    let v1 = eval f1 in
    let v2 = eval f2 in
    match v1, v2 with
    | Bool (TRUE), Bool (FALSE) -> Bool (FALSE)
    | _ -> Bool (TRUE)
  | LESS (e1, e2) ->
    let i1 = calc e1 in
    let i2 = calc e2 in
    if e1 < e2 then Bool (TRUE) else Bool (FALSE)