(* 1 - Boite a outils
1.1 - Basics *)

(* OUTIL 1 - length *)

let rec length = function
  |[] -> 0
  |e::l -> 1 + length l;;

(* OUTIL 2 - nth *)

let nth n list =
  if n < 0 then
    invalid_arg "The index must be a natural !"
  else
let rec recnth = function
  |(_,[]) -> failwith "The list is too short for this argument !"
  |(1,e::l) -> e
  |(i,e::l) -> recnth ((n-1),l)
in recnth (n,list);;

(* OUTIL 3 - is_pos *)

let rec is_pos = function
  |[] -> true
  |e::l -> if e >= 0 then
             is_pos l
           else false;;

(* OUTIL 4 - get_max *)

let rec get_max = function
  |e::[] -> e
  |e::l -> let maxaux = max l in if e > get_max then e else max
  |[] -> invalid_arg "The given list is empty"
    in match list with
       |[] -> invalid_arg "The given list cannot be empty"
       |e::l -> get_max l;;

(* 1.2 - Construire - Modifier *)
(* OUTIL 5 - init_list *)

let rec init n x = function
  |n when n < 0 -> invalid_arg "The given n must be a natural"  
  |0 -> []
  |_ -> x::l && init (n-1) x
in init n x [];;

(* OUTIL 6 - append *)

let rec append l1 l2 = match l1 with
  |[] -> l2
  |e::l -> e::append l l2;;

(* OUTIL 7 - put_list *)

let put_list x i list =
  if i <= 0 then
    invalid_arg "The given position must be a natural"
  else if [] = list then failwith "The given list cannot be empty"
  else let rec putlistrec = function
         |1 -> x::list
         |_ -> e::putlistrec x (i-1) list
       in putlistrec x i list;;

(* 1.3 - 'a list list *)
(* OUTIL 8 - init_board *)

let init_board (l,c) val =  ;;

(* OUTIL 9 - is_board *)

let rec is_board board =
  let (e::l) = board function
  |[] -> true
         |;;

(* OUTIL 10 - print_board *)
