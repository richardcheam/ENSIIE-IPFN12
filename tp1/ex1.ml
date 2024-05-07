(* 1. Calculer (et afficher) une valeur approchée de π à l'aide de la fonction arctangente *)

let pi = 4. *. atan(1.);;
Format.printf "%f\n" pi;;

(* 2. Calculer le volume d'une sphère en fonction d'un rayon passé en paramètre *)

let volume_sphere = fun r -> (4. *. pi *. (r *. r *. r)) /. 3.;;
let res_vol = volume_sphere 5.;; (* pour r = 5 *)
Format.printf "%f\n" res_vol;;

(* 3.1 Une nouvelle fonction arithmétique : mod *)

let check_pair = fun num ->
    if num mod 2 == 0 
    then true
    else false
    ;;

check_pair 4;;
check_pair 3;;

(* 3.2 Proposer une fonction qui affiche Pair si son paramètre est pair, Impair si son paramètre est impair *)

let check_pair_print = fun num ->
    if num mod 2 == 0
    then Format.printf "Pair\n"
    else Format.printf "Impair\n"
    ;;

check_pair_print 2;;
check_pair_print 3;;

(* 3.3 Proposer une fonction qui affiche L'entier n est pair. si son paramètre n est pair, etc *)

let check_pair_print_num = fun num ->
    if num mod 2 == 0
    then Format.printf "L'entier %i est pair\n" num
    else Format.printf "L'entier %i est impair\n" num
    ;;

check_pair_print_num 5;;

(* 4. Proposer une fonction qui, sur la donnée de trois entier c1, c2 et h teste s'ils peuvent représenter les longueurs des côtés d'un triangle rectange d'hypothénuse de longueur h *)

let check_hypo = fun c1 c2 h -> c1*c1 + c2*c2 == h*h;;

check_hypo 3 4 5;;

(* 5. Proposer une fonction qui, sur la donnée de trois entier c1, c2 et c3, teste s'ils peuvent représenter les longueurs des côtés d'un triangle rectange *)

let check_triangle = fun c1 c2 c3 ->
    if (c1*c1 + c2*c2 == c3*c3 ||
        c1*c1 + c3*c3 == c2*c2 ||
        c2*c2 + c3*c3 == c1*c1 )
    then true
    else false
    ;;

check_triangle 3 4 5;;

(* 6. Proposer une fonction qui, sur la donnée d'un pas dx, d'une fonction et d'un point x, calcule une approximation de la dérivée de la fonction en ce point *)
(* step function point *)

let valeur_derivee = fun dx f x -> (f (x + dx) - f x ) / dx;;

let f x = 2 * x * x;;