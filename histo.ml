(* 2 - Midterms... *)
(* 2.1 - Chargement de fichier *)

#use "list_tools.ml";;

(* FONCTION 1 - add_occ *)

let rec add_occ i hist =
  if i <= 0 then
    invalid_arg "The given position must be a natural"
  else if [] = hist then
    failwith "The argument hist is too short"
  else let rec addocc = function
         |(0,e::l) -> e+1::l
         |(i,e::l) -> e::addocc (i-1,l)
         |(i,[]) -> failwith ""
       in addocc (i,hist);;

(* FONCTION 2 - get_hist *)

let get_hist hist =
let rec search = function
  | [] -> []
  | e::l -> add_occ e hist ; search hist (x+1)
in search hist x*

(* FONCTION 3 - get_sorted *)

let get_sorted 