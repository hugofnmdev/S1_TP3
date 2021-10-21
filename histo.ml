(* 2 - Midterms... *)
(* 2.1 - Chargement de fichier *)

#use "list_tools.ml";;

(* FONCTION 1 - add_occ *)

let rec add_occ i hist =
  if i <= 0 then
    invalid_arg "The given position must be a natural"
  else if [] = hist then
    failwith "The argument hist is too short"
  else let rec addocc = match i with
         |1 -> i::hist
         |[_] -> e::addocc (i-1) hist
       in insertrec i hist;;

(* FONCTION 2 - get_hist *)

let get_hist list =
let rec gethist = function
  |[] -> []
  |e::l when e = n -> 1 + gethist n list
  |_::l -> gethist n l
in gethist n list;;
