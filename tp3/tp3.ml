(* ---------- TP3 ---------- *)

let rec nbbase b n = 
	if n < b
		then 1
		else 1 + nbbase b (n/b);;

(* Format.printf("write n in base b : %d\n") (nbbase 12 256);; *)

(* Manipulation de listes *)

let rec recherche = fun n l ->
  match l with 
  |[] -> false
  |h :: t -> 
    if (n = h) then true
    else (recherche n t)
;;

(* Format.printf("recherche: %b\n") (recherche 2 [1; 2; 3]);; (* recherche pour l'entier 2 dans la liste [1;2;3]*)
Format.printf("recherche: %b\n") (recherche 4 [1; 2; 3]);; (*recherche pour l'entier 4 dans la liste [1;2;3]*) *)

let rec nb_ocurrences = fun e l ->
  match l with 
  |[] -> 0
  |h :: t ->
    if (e = h) then 1 + (nb_ocurrences e t)
    else (nb_ocurrences e t)
;;

(* Format.printf("nb_ocurrences: %d\n") (nb_ocurrences 3 [1; 2; 3; 3]);; *)

let rec supprime_tout = fun n l ->
  match l with
  |[] -> []
  |h :: t ->
    if (n = h) then (supprime_tout n t)
    else h::(supprime_tout n t)
;;

(* Format.printf ("supprime_tout: ");;
List.iter (Format.printf "%d ") (supprime_tout 2 [1; 2; 2; 3]);; (* delete 2 from list *)
Format.printf ("\n");; *)

let inv = fun list -> 
  let rec inverse = fun acc l ->
    match l with
    |[] -> acc
    |h :: t -> inverse (h::acc) t in
  inverse [] list (* call an empty list to be an accumulator *)
;;

(* Format.printf ("inverse: ");;
List.iter (Format.printf "%d ") (inv [1; 2; 3]);; (* expect [3; 2; 1] *)
Format.printf ("\n");; *)

let check_pair = fun n ->
  if (n mod 2) == 0 then true
  else false
;;

let rec existe = fun p l -> (* p is a function, so the function return true if we apply the p function on each element of the list and it returns true *)
  match l with
  |[] -> false
  |h :: t -> if p h then true else existe p t
;;

let existe = fun p l -> (* p is a function, so the function return true if we apply the p function on each element of the list and it returns true *)
  let rec existe_aux = fun p l ->
    match l with
    |[] -> false
    |h :: t -> if p h then true else existe p t
  in
  match l with
  | [] -> failwith "empty"
  | _ -> existe_aux p l
;;

(* Format.printf("%b\n") (existe check_pair [2;2;4]);;
Format.printf("%b\n") (existe check_pair [1;5;3]);;
Format.printf("%b\n") (existe check_pair []);; *)

let recherche_tous_k = fun k l ->
  let rec recherche_all = fun acc k l ->
    match l with 
    |[] -> acc
    |h :: t ->
      if k = (fst h) then recherche_all ((snd h)::acc) k t
      else recherche_all acc k t
    in 
    recherche_all [] k l
;;

(* let l = [("London", 4); ("Paris", 13); ("Paris", 7)];;
Format.printf ("recherche_tous_k: ");;
List.iter (Format.printf "%d ") (recherche_tous_k "Paris" l);;
Format.printf ("\n");; *)


(* -------------- En utilisant fold_left, fold_right --------------- *)

let recherche_fold = fun n l ->
  List.fold_left (fun acc m -> if m = n then true else false) false l
;;

(* Format.printf("recherche_fold: %b\n") (recherche_fold 3 [1;2;3]);;
Format.printf("recherche_fold: %b\n") (recherche_fold 13 [1;2;3]);; *)

let nb_occurrences_fold = fun n l ->
  List.fold_left (fun acc m -> if m = n then acc + 1 else acc) 0 l
;;

(* Format.printf("nb_occurrences_fold: %d\n") (nb_occurrences_fold 2 [1;2;3;2;3;2]);; *)


(* let supprime_tout_fold = fun n l ->
  List.fold_left (fun acc h -> if h = n then acc else h::acc) [] l |> List.rev 
;; *)

let supprime_tout_fold = fun n l ->
  List.fold_right (fun h acc -> if h = n then acc else h::acc) l [] 
;;

(* Format.printf ("supprime_tout_fold: ");;
List.iter (Format.printf "%d ") (supprime_tout_fold 2 [1;2;2;3]);;
Format.printf ("\n");;  *)

let inverse_fold = fun l ->
  List.fold_left (fun acc h -> h::acc) [] l 
;;

(* Format.printf ("inverse_fold: ");;
List.iter (Format.printf "%d ") (inverse_fold [1;2;3]);;
Format.printf ("\n");;  *)

let existe_fold = fun p l ->
  List.fold_left (fun acc h -> if p h then true else false) false l
;;

(* Format.printf("%b\n") (existe_fold check_pair [2;2;4]);;
Format.printf("%b\n") (existe_fold check_pair [1;5;3]);;
Format.printf("%b\n") (existe_fold check_pair []);; *)

let recherche_tous_k_fold = fun k l ->
  List.fold_left (fun acc h -> if k = (fst h) then (snd h::acc) else acc) [] l
;;

(* let l = [("London", 4); ("Paris", 13); ("Paris", 7)];;
Format.printf ("recherche_tous_k: ");;
List.iter (Format.printf "%d ") (recherche_tous_k_fold "Paris" l);;
Format.printf ("\n");; *)


(* ----------------------- ITÉRATEURS ----------------------- *)

let rec scanl = fun f l v0 ->
    match l with 
    |[] -> [v0]
    |h :: t -> v0::(scanl f t (f v0 h))
;;

Format.printf ("scanl: ");;
List.iter (Format.printf "%d ") (scanl (fun x y -> x + y) [1;2;3;4;5] 0);;
Format.printf ("\n");; 

(* let scanl_fold = fun f l v0 -> (* not yet correct cos cannot update v0 *)
    List.fold_left 
      (
        fun acc x -> match acc with 
        |[] -> []
        |h::t -> (f h (f v0 x))::acc
      ) 
      [v0] 
      l|> List.rev
;; *)

let scanl_fold = fun f l v0 ->
  List.fold_left (fun acc h -> (f h (List.hd acc))::acc) [v0] l |> List.rev
;;

Format.printf ("scanl: ");;
List.iter (Format.printf "%d ") (scanl_fold (fun x y -> x + y) [1;2;3;4;5] 0);;
Format.printf ("\n");; 

(* -------------- ZIPPER -------------- *)

type 'a zipper = {
  left : 'a list;
  right : 'a list;
}

let to_zip l = {
  left = [];
  right = l;
}

let to_list z = List.rev_append z.left z.right;;

let move_right = fun z ->
  match z.right with
  |[] -> invalid_arg "move_right"
  |h :: t -> 
    { 
      left = (h :: z.left) ;
      right = t;
    }
;;

let move_left = fun z ->
  match z.left with
  |[] -> invalid_arg "move_left"
  |h :: t ->
    {
      left = t ;
      right = (h :: z.right) ;
    }
;;

let insert_left = fun v z -> { z with left = v::z.left };; (* return a z with modified z.left to v::z.left *)

let insert_right = fun v z -> { z with right = v::z.right};;

let delete_left = fun z -> 
  match z.left with 
  |[] -> invalid_arg "delete_left"
  |_ :: t -> { z with left = t}
;;

let delete_right = fun z ->
  match z.right with
  |[] -> invalid_arg "delete_right"
  |_ :: t -> { z with right = t}
;;

let to_start = fun z -> { left = [] ; right = List.rev_append z.left z.right};;

let to_end = fun z -> { left = List.rev_append z.right z.left ; right = [] };;