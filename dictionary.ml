(* type key = int;; *)
type 'a assoc = (int * 'a) list;;

let init_assoc() : 'a assoc = [];;

let is_empty = fun (a: 'a assoc) ->
  match a with
  |[] -> true 
  |_ -> false;;

let add = fun k v (m: 'a assoc) -> (k, v)::m;;

let rec modif = fun k new_v (m: 'a assoc) -> 
  match m with
  |[] -> []
  |(cle, valeur)::t -> 
      if cle = k then (cle, new_v)::t
      else modif k new_v t;;

let rec search = fun k (m: 'a assoc) -> 
  match m with
  |[] -> failwith "il n'y a pas la valeur qui correspond à cette cle"
  |(cle, v)::t -> 
      if k = cle then v 
      else search k t;;

let rec delete = fun k (m: 'a assoc) -> 
  match m with
  |[] -> []
  |(key, value)::t -> 
    if key = k then t
    else delete k t;;

let fold_assoc = fun f (m: 'a assoc) ->
  List.fold_left f ([]: 'a assoc) m;;

let rec increment_key = fun (m: 'a assoc) ->
  match m with
  |[] -> []
  |(k, v)::t -> ((k+1, v)::(increment_first_key t) : 'a assoc);;

let my_dic = init_assoc();; 
is_empty my_dic;;

let my_dic = add 1 "Paris" my_dic;; 
let my_dic = add 2 "Versaille" my_dic;;
let my_dic = modif 2 "Lyon" my_dic;;
let my_dic = delete 2 my_dic;;

fold_assoc (fun acc (a, b) -> (a+1, b)::acc) my_dic |> List.rev;;
