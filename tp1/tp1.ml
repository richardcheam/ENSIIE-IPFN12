(* ------------------------ex1--------------------------- *)

(* 1 *)

let pi = atan(1.) *. 4.;;    (* add . at the end of a number or variable in order to notify that it is a float type*)
(*
Format.print_float pi;;
Format.printf("\n");;
*)

(* 2 *) 

let volume_sphere = fun r -> (4. *. pi *. (r *. r *. r)) /. 3.;;
(* Format.printf "sphere volume of radius 4 = %f\n" (volume_sphere 4.);; *)

(* 3 *)

let check_pair_1 = fun x ->
  if x mod 2 = 0
    then true
  else false
;;

let check_pair_2 = fun x ->
  if x mod 2 = 0
    then Format.printf "Pair\n"
  else Format.printf "Impair\n"
;;

let check_pair_3 = fun x ->
  if x mod 2 = 0
    then Format.printf "%d est pair\n" x
  else Format.printf "%d est impair\n" x
;;

(* 4 *)

let check_hypo_1 = fun c1 c2 h -> 
  if ((c1*c1) + (c2*c2)) = (h*h)
    then true
  else false
;;
(* 
Format.printf "%b\n" (check_hypo_1 3 4 5);; 
Format.printf "%b\n" (check_hypo_1 2 2 4);;
*)

(* 5 *)

let check_hypo_2 = fun c1 c2 c3 ->
  if  ((c1*c1) + (c2*c2) = (c3*c3) ||
      (c1*c1) + (c3*c3) = (c2*c2) ||
      (c2*c2) + (c3*c3) = (c1*c1))
    then true
  else false
;;
(*
Format.printf "%b\n" (check_hypo_2 3 4 5);; 
Format.printf "%b\n" (check_hypo_2 2 2 4);;
*)

(* 6 *)

let approx_derive = fun dx f x -> (f (x + dx) - f (x)) / dx;;
(* 
let f x = 2 * x * x;;
Format.printf "%d\n" (approx_derive 2 f 2);;
*)

(* 7 *)

let compos_fonc = fun f g x -> f(g x);;
(* 
let f x = 1 + x;;
let g x = x * x;;
Format.printf "%d\n" (compos_fonc f g 2);; pour valeur x = 2 
*)

(* ------------------------ex2--------------------------- *)

(* 1 *)
let swap = fun (a, b) -> (b, a);; (* OR let swap (a, b) = (b, a) *)
(*
let (first, second) = (2 , 5);;
Format.printf "before swap : (%d, %d)\n" first second;;
let (first, second) = swap (first, second);;
Format.printf "after swap : (%d, %d)\n" first second;;
*)

(* 2 *)
let return_first_arg = fun a b -> a;; 
(* Format.printf "%d\n" (return_first_arg 2 3);; *)

(* 3 *)
let change_arg_f = fun f b a -> f a b;;
(* let subtract a b = a - b;;
Format.printf("%d\n") (change_arg_f subtract 2 5);; (* it changes the input argument from 2 5 to 5 2 *)
*)

(* 4 *)
let apply_on_fst_tuple = fun f (a,c) -> (f a, c);; (* apply f to the first element of a pair *)
(* let f x = 1 + x;; (* let f = fun x -> 1 + x *)
let test_pair = (1, "Bon");;
let res = apply_on_fst_tuple f (test_pair);;
Printf.printf"(%d, %s)\n" (fst res) (snd res);;   *)

(* 5 *)
let five = fun f g a -> 
  let b = f a in
  g b a
;;
(* let f x = 1 + x;;
let g a b = a + b;;
Format.printf("%d\n") (five f g 2);; *)


(* ------------------- ex3 -------------------- *)
let rec pow = fun x p ->
  match p with
  |_ when p < 0 -> invalid_arg "p must be greater than 0"
  |0 -> 1
  |_ -> x * (pow x (p-1))
;;

let rec pow_2 = fun x p ->
  if p<0 then failwith "p must be greater 0"
  else if p == 0 then 1
  else x * (pow x (p-1))
;;

Printf.printf("%d\n") (pow 2 10);;
Printf.printf("%d\n") (pow 2 100);; (* grand p => resultat = 0 *)
(* Printf.printf("%d\n") (pow_2 2 (-13));; *)

let rec pow_opt = fun x p ->
  if p < 0 then invalid_arg "p must be greater than 0 "
  else if p = 0 then 1
  else if p > 0 && p mod 2 = 0 then ((pow_opt x (p/2)) * (pow_opt x (p/2)))
  else x * ( (pow_opt x ((p-1)/2)) * (pow_opt x ((p-1)/2)) )
;;

Printf.printf("%d\n") (pow_opt 2 10);;
Printf.printf("%d\n") (pow_opt 2 20);; 