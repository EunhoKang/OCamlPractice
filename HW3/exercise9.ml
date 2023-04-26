type formula = TRUE | FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec calc : expr -> expr
=fun ex ->
  match ex with
  | NUM e -> NUM e
  | PLUS (e1, e2) -> ( 
    let i1 = calc e1 in
    let i2 = calc e2 in
    match i1, i2 with
    | NUM i1, NUM i2 -> NUM (i1 + i2)
    | _ -> NUM 0
    )
  | MINUS (e1, e2) -> (
    let i1 = calc e1 in
    let i2 = calc e2 in
    match i1, i2 with
    | NUM i1, NUM i2 -> NUM (i1 - i2)
    | _ -> NUM 0
  )

let rec eval : formula -> formula
=fun fm ->
  match fm with
  | TRUE -> TRUE
  | FALSE -> FALSE
  | NOT (f) -> (
    let v = eval f in
    match v with
    | TRUE -> FALSE
    | FALSE -> TRUE
    | _ -> TRUE
  )
  | ANDALSO (f1, f2) -> (
    let v1 = eval f1 in
    let v2 = eval f2 in
    match v1, v2 with
    | TRUE, TRUE -> TRUE
    | _ -> FALSE
  )
  | ORELSE (f1, f2) -> (
    let v1 = eval f1 in
    let v2 = eval f2 in
    match v1, v2 with
    | FALSE, FALSE -> FALSE
    | _ -> TRUE
  )
  | IMPLY (f1, f2) -> (
    let v1 = eval f1 in
    let v2 = eval f2 in
    match v1, v2 with
    | TRUE, FALSE-> FALSE
    | _ -> TRUE
  )
  | LESS (e1, e2) -> (
    let i1 = calc e1 in
    let i2 = calc e2 in
    if i1 < i2 then TRUE else FALSE
  );;