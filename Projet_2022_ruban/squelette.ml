
(** 
    Cette partie du programme NE DOIT EN AUCUN CAS être modifiée sous peine
    de voir votre code ne pas passer les tests 
*)
type instruction = (* représentation d'une instruction reçu *)
  | Left (* Déplacement du curseur vers la gauche *)
  | Right (* Déplacement du curseur vers la droite *)
  | Write of char (* Écriture du caractère sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  | Caesar of int (* Caesar n à partir de la phase 3Aiens *)
  | Delete of char (* Delete(a) suppression du caractère a du message à partir de la phase 3Aiens *)
  | Invert (* Invert : retournement du message  à partir de la phase 3Aiens *)
  
type program = instruction list (* Un programme est simplement une liste d'instruction *)

(**
   VOUS N'AVEZ PAS BESOIN DE LIRE OU DE COMPRENDRE CETTE PARTIE DU CODE (LES SIX FONCTIONS SUIVANTES). 
   ELLE EST VOLONTAIREMENT NON COMMENTÉE ET RÉDIGÉE DE MANIÈRE PEU CLAIRE 
 *)
let rec analyse_comment fd =
  let c = Scanf.bscanf fd "%c" (fun c -> c) in
  if c = '\n'
  then ()
  else analyse_comment fd
             
let rec analyse_program_aux =
  fun fd lvl  ->
  try 
    let c = Scanf.bscanf fd "%c" (fun a  -> a) in
    if c = '#'
    then
      let _ = analyse_comment fd in
      analyse_program_aux fd lvl
    else if c = ';' || c = '\n' || c = ' '
    then analyse_program_aux fd lvl
    else if c = ']'
    then
      if lvl > 0
      then []
      else raise (Invalid_argument "Error on char ]")
    else if c = 'W'
      then
        let i= Write(Scanf.bscanf fd "(%c)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'C'
      then
        let i= Caesar(Scanf.bscanf fd "(%d)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'D'
    then
      let a = Scanf.bscanf fd "(%c)" (fun a -> a) in 
      let i= Delete(a) in
      let li = analyse_program_aux fd lvl in
      i::li
    else if c = 'R'
    then let li = analyse_program_aux fd lvl in
         Right::li
    else if c = 'I'
    then Invert::analyse_program_aux fd lvl
    else if c = 'L'
    then Left::analyse_program_aux fd lvl
    else if c = 'F'
    then
      let n = Scanf.bscanf fd "(%d,[" (fun n -> n) in
      let l = analyse_program_aux fd (lvl + 1) in
      let c = Scanf.bscanf fd "%c" (fun a -> a) in
      if c <> ')' then raise (Invalid_argument ("Error found '"^String.make 1 c^"' instead of ')'"))
      else
        let li = analyse_program_aux fd lvl in
        Repeat(n,l)::li
    else
      let _ = Format.printf  "ERROR '%c' (%d)@." c (Char.code c) in
      assert false
  with End_of_file -> []

let rec read_file_aux =
  fun acc fd ->
  try
    let c = Scanf.bscanf fd "%c" (fun x -> x) in
    read_file_aux (c::acc) fd
  with End_of_file -> acc

let read_file file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      List.rev (read_file_aux [] fd)
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127

                    
let rec fprintf_instruction fmt i =
  match i with
  | Write c -> Format.fprintf fmt "W(%c)" c
  | Right -> Format.fprintf fmt "R"
  | Left -> Format.fprintf fmt "L"
  | Repeat(n,li) ->
     Format.fprintf fmt "F(%d,[%a])" n (fun fmt li -> List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) li) li
  | Caesar n -> Format.fprintf fmt "C(%d)" n
  | Delete(a) -> Format.fprintf fmt "D(%c)" a
  | Invert -> Format.fprintf fmt "I"
            
let fprintf_program fmt l =
  List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) l

(*** 
     Retour au côté clair
*)


(* 
   [print_program] : program -> unit 
   affiche un [program] sur la sortie standard de l'executable
 *)  
let print_program p = Format.printf "%a" fprintf_program p

(*
  [analyse_program] : unit -> program
  Lit un programme 1Aien ou 2Aien, l'analyse et le retourne sous forme d'une valeur de type [program]
 *)                    
let analyse_program file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      analyse_program_aux fd 0
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127









    
(** Votre code doit commencer à partir de ce point.

    NE MODIFIER RIEN AU DESSUS DE CE POINT 
*)

(* Richard CHEAM *)

(* define a zipper type (of char) *)
type ruban = {  
    left : char list;
    right : char list;
}

(*
@type:     ruban -> ruban
@requires: a valid r of char
@ensures:  move the cursor a step to the right
           if the list on the right is not empty, concatenate the first element of the right list to the left list
           if it is empty send back the input r as there is no element on the right
           else error
@raises:   nothing
*)
let move_right = fun r ->
  match r.right with
  |[] -> r
  |h :: t -> 
    { 
      left = (h :: r.left);
      right = t;
    }
;;

(*
@type:     ruban -> ruban
@requires: a valid r of char
@ensures:  move the cursor a step to the left
           if the list on the left is not empty, concatenate the first element of the left list to the right list
           if it is empty send back the input r as there is no element on the left
           else error
@raises:   nothing
*)
let move_left = fun r ->
  match r.left with
  |[] -> r
  |h :: t ->
    {
      left = t;
      right = (h :: r.right);
    }
;;

(* or r with right = c::r.right / we can also concatenate on the left it works the same way *)
(*
@type:     char -> ruban -> ruban
@requires: a valid r of char
@ensures:  concatenation of the input char c onto the ruban r (write c on r) if it was used correctly, otherwise error
@raises:   nothing
*)
let write = fun c r -> { left = r.left ; right = c::r.right };; 

(*
@type:     ruban -> ruban
@requires: a valid r of char
@ensures:  move every element of the left to the start of right list if the function was used properly, otherwise error
           as the head of the left list is at the tail, we need to reverse it first before append
@raises:   nothing
*)
let rub_to_start = fun r -> {left = []; right = List.rev_append r.left r.right};;

(*
@type:     ('a -> char -> 'a) -> 'a -> ruban -> 'a
@requires: a valid r of char
@ensures:  fold a ruban to a single element (type v0) via an operation f
@raises:   "No message" if the input ruban r is empty (as we moved everything onto the right)
*)
let fold_ruban = fun f v0 r -> 
  let _r = rub_to_start r in
    match _r.right with
    |[] -> failwith "No message"
    |l -> (* convert _r.right to list and then it will work as fold_left (list) *)
      let rec fold_ruban_aux = fun f v0 l ->
        match l with 
        |[] -> v0
        |h :: t -> fold_ruban_aux f (f v0 h) t 
      in fold_ruban_aux f v0 l
  ;;

(* https://www.youtube.com/watch?v=ergRKv3DglI&t=459s *)
(* https://v2.ocaml.org/api/Char.html *)
(* https://stackoverflow.com/questions/22208139/c-caesar-cipher-ascii-alphabet-wrap *)
(*
@type:     int -> char -> char
@requires: nothing
@ensures:  the application of Caesar encoding method n step(s) on a char c if the arguments were passed correctly, otherwise error
@raises:   nothing
*)
let _caesar = fun n c ->
  let i = Char.code c in 
    if (i >= 97 && i <= 122) then (* check if they are lowercase alphabet from a-z in ASCII *)
      let to_upper = Char.uppercase_ascii c in (* convert the input char c to upper becos the procedure of shifting only works with uppercase alphabet *)
      let _i = Char.code to_upper in
      let tmp1 = _i - 65 in
      let tmp2 = (tmp1 + n) mod 26 in 
      let res_int = tmp2 + 65 in
      let res_char = Char.chr res_int in 
      Char.lowercase_ascii res_char (* convert it back to lower *)
    else 
      let tmp1 = i - 65 in
      let tmp2 = (tmp1 + n) mod 26 in 
      let res = tmp2 + 65 in
      Char.chr res
;;

(*
@type:     int -> ruban -> ruban
@requires: a valid r of char
@ensures:  the application of Caesar encoding to every element of ruban r if was used correctly, otherwise error
@raises:   "No message" if the input ruban r is empty
*)
let caesar = fun n r ->
  fold_ruban (fun acc h -> let c = _caesar n h in {left = c::acc.left; right = acc.right}) {left = []; right = []} r
;;

(*
@type:     char -> ruban -> ruban
@requires: a valid r of char
@ensures:  the deletion of every occurence of the input char a in ruban r if was used properly, otherwise error
@raises:   "No message" if the input ruban r is empty (as we moved everything onto the right)
*)
let delete = fun a r ->
  fold_ruban (fun acc h -> if a = h then {left = acc.left; right = acc.right} else {left = h::acc.left; right = acc.right}) {left = []; right = []} r
;;

(*
@type:     ruban -> ruban
@requires: a valid r
@ensures:  the inversion of the input ruban r if was used properly, otherwise error
@raises:   nothing
*)
let invert = fun r -> {left = r.right; right = r.left};; 

(*
@type:     program -> ruban
@requires: a valid p
@ensures:  return the ruban which were applied instructions on if it was used correctly, else error
@raises:   "No instruction" if the input list of instruction p is empty
*)
let execute_program = fun p ->
  match p with
  |[] -> failwith "No instruction"
  |h :: t ->
    let rec execute_program_aux = fun p r ->
      match p with 
      |[] -> r
      |h :: t -> 
        match h with 
        |Left -> execute_program_aux t (move_left r)
        |Right -> execute_program_aux t (move_right r)
        |Caesar n -> execute_program_aux t (caesar n r)
        |Delete(c) -> execute_program_aux t (delete c r)
        |Invert -> execute_program_aux t (invert r)
        |Repeat(n,li) -> 
          let rec repeat = fun n li r -> 
            if n = 0 then r 
            else repeat (n-1) li (execute_program_aux li r)
          in execute_program_aux t (repeat n li r)
        |Write c -> 
          match t with (* check if the next instruction is Write as well, meaning we have not moved yet *)
          |Write c' :: _ -> execute_program_aux (List.tl t) (write c' r) (*List.tl sends back the input list without its head element*)
          |_ -> execute_program_aux t (write c r)
    in execute_program_aux p {left = []; right = []}
;;

(* I implemented this function in the wrong way, the right one is below *)
(* This function works fine, but it does not read a char list of a plain text, but instead a char list of instruction *)
(* 
@type:     char list -> program
@requires: nothing
@ensures:  the conversion of the input list of char l into a list of instruction if was used properly, else error
@raises:   nothing
*)
(* let generate_program = fun l ->
  let rec generate_program_aux = fun acc l ->
    match l with
    |[] -> acc
    |'#'::t -> 
      let rec skip = fun x ->
        match x with 
        |[] -> []
        |'\n' :: xs -> xs (* if we encounter a break line (\n) we stop and send back the list of char *)
        |_ :: xs -> skip xs (* if not we keep searching until we find '\n' *)
      in generate_program_aux acc (skip t)
    |'L'::';'::t | 'L'::t -> generate_program_aux (Left::acc) t
    |'R'::';'::t | 'R'::t -> generate_program_aux (Right::acc) t
    |'I'::';'::t | 'I'::t -> generate_program_aux (Invert::acc) t 
    |'W'::'('::c::')'::';'::t | 'W'::'('::c::')'::t -> generate_program_aux ((Write c)::acc) t
    |'D'::'('::c::')'::';'::t | 'D'::'('::c::')'::t -> generate_program_aux ((Delete(c))::acc) t
    |'C'::'('::n::')'::';'::t | 'C'::'('::n::')'::t -> generate_program_aux ((Caesar ((int_of_char n) - (int_of_char '0')))::acc) t
    |'F'::'('::n::','::'['::t -> 
      let rec generate_program_aux_aux = fun acc2 l2 ->
        match l2 with
        |[] -> (acc2, [])
        |'L'::';'::t2 | 'L'::t2 -> generate_program_aux_aux (Left::acc2) t2
        |'R'::';'::t2 | 'R'::t2 -> generate_program_aux_aux (Right::acc2) t2
        |'I'::';'::t2 | 'I'::t2 -> generate_program_aux_aux (Invert::acc2) t2 
        |'W'::'('::c::')'::';'::t2 | 'W'::'('::c::')'::t2 -> generate_program_aux_aux ((Write c)::acc2) t2
        |'D'::'('::c::')'::';'::t2 | 'D'::'('::c::')'::t2 -> generate_program_aux_aux ((Delete(c))::acc2) t2
        |'C'::'('::n::')'::';'::t2 | 'C'::'('::n::')'::t2  -> generate_program_aux_aux ((Caesar ((int_of_char n) - (int_of_char '0')))::acc2) t2
        |']'::')'::';'::t2 | ']'::')'::t2 -> (acc2, t2)
        |_ :: t2 -> generate_program_aux_aux acc2 t2 
      in
      let cte = generate_program_aux_aux [] t
      in generate_program_aux ((Repeat((int_of_char n) - (int_of_char '0') , (List.rev (fst cte))))::acc) (snd cte)
    |_ :: t -> generate_program_aux acc t
  in List.rev (generate_program_aux [] l)
;;  *)

(* 
@type:     'a list -> int
@requires: nothing
@ensures:  return the number of a repetition of an element in the input list, meaning consecutive element
           it works only for when the consecutive element is at the beginning of the list (explanation is in report)
@raises:   nothing
*)
let rec count_consecutive = fun l ->
  match l with 
  | [] -> 0
  | [h] -> 1
  | h1 :: h2 :: t ->
    if h1 = h2 then 1 + count_consecutive (h2 :: t)
    else 1
;;

(* 
@type:     int -> 'a list -> 'a list
@requires: nothing
@ensures:  return the input list without the first n elements 
@raises:   nothing
*)
let rec remove_element n l =
  if n <= 0 then l
  else match l with
    | [] -> []
    | _ :: t -> remove_element (n-1) t
;;

(* 
@type:     char list -> program
@requires: nothing
@ensures:  the conversion of the input list of char l into a list of instruction if was used properly, else error
@raises:   nothing
*)
let generate_program = fun l ->
  let rec generate_program_aux = fun acc l ->
    match l with
    |[] -> acc
    |h :: t -> 
      if t = [] then ([Right; Write h]@acc) (* if we don't put this case it will be an exception hd *)
      else if h = (List.hd t) then 
        let count = count_consecutive l in
        generate_program_aux ((Repeat(count,[Write h; Right]))::acc) (remove_element count l)
      else generate_program_aux ([Right; Write h]@acc) t
  in List.rev (generate_program_aux [] l)
;;

      

(** Votre code doit s'arreter à partir de ce point.

    NE MODIFIER RIEN EN DESSOUS DE CE POINT 
 *)











                         
                         
let die i = 
  let _ = Format.fprintf Format.err_formatter "Usage: %s <1|2|3> <file>@." Sys.argv.(0) in
  exit i
  
let main phase file =
  if phase = "1" || phase = "2"
  then  
    let li = analyse_program file in
    let rub = execute_program li in
    let _ = fold_ruban (fun _ c -> Format.printf "%c" c) () rub in
    Format.printf "@."
  else if phase = "3"
  then
    let msg = read_file file in
    let p = generate_program msg in
    print_program p
  else die 1




let _ =
  if Array.length Sys.argv = 3 
  then
    main Sys.argv.(1) Sys.argv.(2)
  else die 2
