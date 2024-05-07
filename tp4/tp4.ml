type quad = Black | White | Node of quad * quad * quad * quad

(* -------------- Exercice 1 - Arbres quaternaires -------------- *)

(* Node (1,2,3,4) *)

let rec mirrorV = fun q ->
  match q with
  |Black -> Black
  |White -> White
  |Node (nw, ne, se, sw) -> Node(mirrorV ne, mirrorV nw, mirrorV sw, mirrorV se)
;;

let rec rotate = fun q -> 
  match q with
  |Black -> Black
  |White -> White
  |Node (nw, ne, se, sw) -> Node(rotate ne, rotate se, rotate sw, rotate nw)
;;

let rec density = fun q ->
  match q with
  |Black -> 1.000
  |White -> 0.000
  |Node (nw, ne, se, sw) -> (density nw +. density ne +. density se +. density sw) /. 4.000
;;

let rec apply = fun f q ->
  match f with
  |[] -> q
  |h::t -> 
    let q1 = h q in (apply t q1) (* or h::t -> apply t (h q) *)
;;

let rec rotate_clockwise = fun q ->
  match q with
  |Black -> Black
  |White -> White
  |Node (nw, ne, se, sw) -> Node (apply [rotate_clockwise] sw, apply [rotate_clockwise] nw, apply [rotate_clockwise] ne, apply [rotate_clockwise] se)
;;

let rec mirrorH = fun q ->
  match q with
  |Black -> Black
  |White -> White
  |Node (nw, ne, se, sw) -> Node (apply [mirrorH] sw, apply [mirrorH] se, apply [mirrorH] ne, apply [mirrorH] nw)
;;

(* -------- Exercice 2 - Expressions arithmétiques -------- *)

type expr = 
  | Nb of int | Plus of expr * expr
  | Mult of expr * expr | Minus of expr * expr
;;

let expression = Minus(Mult(Plus(Nb 1, Nb 4),Nb 3), Nb 5);;

(* let nb_op = fun expr -> 
  let rec nb_op_aux = fun acc expr ->
    match expr with
    |Nb _ -> acc
    |_ -> 1+acc
    in
    nb_op_aux 0 expr
;; *)

(* let nb_op = fun expr -> 
  let rec nb_op_aux = fun acc expr ->
    match expr with
    |Nb _ -> acc
    |Plus _ -> nb_op_aux (1+acc) expr
    |Minus _ -> nb_op_aux (1+acc) expr
    |Mult _ -> nb_op_aux (1+acc) expr
    |_ -> acc
    in
    nb_op_aux 0 expr
;;

let eval = fun expr ->
  let rec eval_aux = fun acc expr -> 
    |Nb _ -> acc
    |Plus _ -> eval_aux (fst Plus + snd Plus) expr
    |Minus _ -> eval_aux (fst Minus - snd Minus) expr
    |Mult _ -> eval_aux (fst Mult * snd Mult) expr
    |_ -> acc
  in
  eval_aux 0 expr
;;  *)