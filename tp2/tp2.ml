(* TP2 *)

(* ----------------Enregistrements et couples---------------- *)

type rationnel_enreg = {num:int ; den:int};;

let add_rationnel_enreg a b = {
  num = (a.num * b.den) + (b.num * a.den) ;
  den = (a.den * b.den)
};;
(*
let a = {num = 1 ; den = 2};;
let b = {num = 1 ; den = 2};;
let res_add_enreg = add_rationnel_enreg a b;;
Format.printf "%d/%d\n" res_add_enreg.num res_add_enreg.den;;
*)

type rationnel_coup = int * int;;

let add_rationnel_coup (a,b) (c,d) = ( (a*d) + (c*b), (b*d) );;
(*
let c1 = (1, 2);;
let c2 = (1, 2);;
let res_add_coup = add_rationnel_coup c1 c2;;
Format.printf "%d/%d\n" (fst res_add_coup) (snd res_add_coup);;
*)

(* ---------------- Types sommes et types énumérés ---------------- *)
(* Mixité entiers/flottants *)

type nombre = Value of int | Valeur of float;;

let un = Value 1;;
let pi = Valeur 3.14159;;

let addition a b =   (* use match OR we can use keyword 'function' instead *)  (* let addition a b = function |Value...*)
  match a, b with
  |Value a, Value b -> Value (a + b)
  |Valeur a, Valeur b -> Valeur (a +. b)
  |Value a, Valeur b -> Valeur (float_of_int a +. b) (* case: 'a' is int , 'b' is float *)  (* int + float = float *)
  |Valeur a, Value b -> Valeur (a +. float_of_int b) (* case: 'a' is float , 'b' is int *)
;;

let subtract a b = 
  match a, b with
  |Value a, Value b -> Value (a - b)
  |Valeur a, Valeur b -> Valeur (a -. b)
  |Value a, Valeur b -> Valeur (float_of_int a -. b)
  |Valeur a, Value b -> Valeur (a -. float_of_int b)
;;

let multiply a b = 
  match a, b with
  |Value a, Value b -> Value (a * b)
  |Valeur a, Valeur b -> Valeur (a *. b)
  |Value a, Valeur b -> Valeur (float_of_int a *. b)
  |Valeur a, Value b -> Valeur (a *. float_of_int b)
;;

let divide a b = 
  match a, b with
  |Value a, Value b -> Value (a / b)
  |Valeur a, Valeur b -> Valeur (a /. b)
  |Value a, Valeur b -> Valeur (float_of_int a /. b)
  |Valeur a, Value b -> Valeur (a /. float_of_int b)
;;

let plus_petit = fun (a: nombre) (b: nombre) ->
  match a, b with
  |Value a, Value b -> if (a > b) then Value b else Value a
  |Valeur a, Valeur b -> if (a > b) then Valeur b else Valeur a
  |Value a, Valeur b -> if (float_of_int a > b) then Valeur b else Valeur (float_of_int a)
  |Valeur a, Value b -> if (a > float_of_int b) then Valeur (float_of_int b) else Valeur a
;;

let print_nombre n =  (* OR let print_nombre = fun n -> *)
  match n with
  |Value n -> Format.printf "%d\n" n
  |Valeur n -> Format.printf "%f\n" n
;;

(*
let res_add = addition un pi;; print_nombre res_add;;
let res_sub = subtract un pi;; print_nombre res_sub;;
let res_mul = multiply un pi;; print_nombre res_mul;;
let res_div = divide un pi;; print_nombre res_div;;
*)

let rec fib_naive = fun n -> (* or let rec fib_naive = n *)
  match n with
  |0 -> 0
  |1 -> 1
  |_-> fib_naive(n-1) + fib_naive(n-2)
;;

let rec fib_naive_2 = fun n ->
  if n = 0 then 0
  else if n = 1 then 1
  else fib_naive_2(n-1) + fib_naive_2(n-2)
;;

(*
Format.printf("%d\n") (fib_naive(10));;
Format.printf("%d\n") (fib_naive_2(10));;
*)

(*
   n -> let (a, b) = fib_aux (n-1) in (a + b, a) computes the n-th and n+1-th
   by first computing the n-1-th and n-th, then adding the n-1-th and n-th
   to obtain the n-th, and returning both the n-th and n+1-th as a tuple.
*)

let rec fib_aux = fun n ->
  match n with
  |0 -> (1, 0)
  |n -> let (a, b) = fib_aux (n-1) in (a + b, a)  (* return (n-th, n-1-th) then (n + n-1, n) *)
;;

let fib = fun n ->
  let (_, res) = fib_aux n in res
;;

(* let res = fib(40);;

Format.printf("%d\n") res;; *)

(* ---------------- Listes Polymorphes ---------------- *)

(* 1 *)
let ajoute = fun n l -> n::l ;;
(* List.iter (Format.printf "%d ") (ajoute 1 [2;3;4]);; Format.printf("\n");; *)

(* 2 *)
let rec recherche = fun n l ->
  match l with
  |[] -> false
  |h :: t -> 
    if (h == n) then true
    else recherche n t
  ;;
(* Printf.printf "%b\n" (recherche 2 [1;2]);;
Printf.printf "%b\n" (recherche 0 [1;2]);; *)

(* 3 *)
let rec existe_pair = fun l ->
  match l with
  |[] -> failwith "Empty list"
  |h :: t ->
    if (h mod 2) == 0 then true
    else if (h mod 2) != 0 then false
    else existe_pair t 
;;
(* Format.printf "%b\n" (existe_pair [1;3;5]);;
Format.printf "%b\n" (existe_pair [2;4;5]);; *)

(* 4 *)
let supprime = fun n l ->
  match l with
  |[] -> failwith "Empty list"
  |h :: t ->
    if (n == h) then t
    else l
;;

(* List.iter (Printf.printf "%d ") (supprime 2 [2; 2; 2; 3]);; 
Printf.printf("\n");; *)

(* 5 *)
let rec supprime_tout = fun n l ->
  match l with
  |[] -> []
  |h :: t ->
    if (h == n) then (supprime_tout n t)
    else h::(supprime_tout n t)
;;

(* Format.printf "[ ";;
List.iter (Format.printf "%d ") (supprime_tout 2 [1;2;2;3]);;
Format.printf "]";;
Format.printf("\n");; *)

(* 6 *)
let rec inverse = fun l ->
  let rec aide = fun acc l ->
    match l with
    |[] -> acc
    |h::t -> aide (h::acc) t in 
  aide [] l
;;

(* Format.printf "[ ";;
List.iter (Format.printf "%d ") (inverse [1;2;3]);;
Format.printf "]";;
Format.printf("\n");; *)

(* 7 *)
let check_impair = fun x ->
  if (x mod 2) == 0 then false
  else true
;;

let rec existe = fun p l -> (* p est une fonction de type bool *)
  match l with
  |[] -> false
  |h::t ->
    if (p h) == true then true (* check all elements *)
    else existe p t
;;

(* Format.printf("%b\n") (existe check_impair [2;2]);;
Format.printf("%b\n") (existe check_impair [1;2]);; *)

