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

let get_hist n list =
let rec gethist = function
  |[] -> []
  |e::l when e = n -> 1 + gethist n list
  |_::l -> gethist n l
in gethist n list;;

(* FONCTION 3 - get_sorted *)

(* FONCTION 4 - hist_sort *)

let rec hist_sort = function (* Outil append dans list_tools.ml *)
  | [] -> []
  | [0] -> [0]
  | e::l ->
      let rec part = function 
        | (l,r,[]) -> append (hist_sort l) (e::hist_sort r)
        | (l,r,h::t) -> if (h <= e)
            then part (h::l,r,t)
            else part (l,h::r,t)
      in 
      part ([],[],l) ;;
