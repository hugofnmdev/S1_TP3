(* 1 - Boite a outils
1.1 - Basics *)

(* OUTIL 1 - length *)

let rec length = function
  |[] -> 0
  |e::l -> 1 + length l;;

(* OUTIL 2 - nth *)

let nth rank list =
  if rank < 0 then
    invalid_arg "The argument rank must be a natural !"
  else
    let rec nth_rec = function
      | (_, []) -> failwith "This list is too short for this argument !"
      | (0, e::l) -> e
      | (i, e::l) -> nth_rec (i-1, l)
    in
    nth_rec (rank, list) ;;

(* OUTIL 3 - is_pos *)

let rec is_pos = function
  |[] -> true
  |e::l -> if e >= 0 then
             is_pos l
           else false;;

(* OUTIL 4 - get_max *)

let rec get_max = function
  |e::[] -> e
  |e::l -> let max = get_max l in
   if e > max then e else max
  |[] -> failwith "The list cannot be empty" ;;

(* 1.2 - Construire - Modifier *)
(* OUTIL 5 - init_list *)

let rec init_list n x = 
  match n with
  |a when n<0 -> invalid_arg "The argument n must be a natural"
  |0 -> []
  |_ -> x :: init_list(n-1) x ;;

(* OUTIL 6 - append *)

let rec append l1 l2 = match l1 with
  |[] -> l2
  |e::l -> e::append l l2;;

(* OUTIL 7 - put_list *)

let rec add_list x = function
        | [] -> [x]
        | e::l when x<e -> x::e::l
        | e::l when x>e -> e::(add_list x l)
        | _ -> failwith "";;

(* 1.3 - 'a list list *)
(* OUTIL 8 - init_board *)

let rec init_board (l,c) v =
  if l<0 && c<0 then
    invalid_arg "The arguments l*v must be naturals !"
  else
   match (l,c) with 
    |(0,_)|(_,0) -> [] 
    |(1,c) -> init_list c v ::[]
    |(_,_) -> init_list c v :: init_board (l-1,c) v ;;

(* OUTIL 9 - is_board *)

let rec is_board list = match list with
  |[] -> true
  |e::[] -> true
  |e::e1::l -> length e = length e1 && is_board (e1::l) ;; (* Length codé en haut du document *)

(* OUTIL 10 - print_board *)

(* OUTIL 11 - get_cell *)

let get_element e l = 
  let rec aux n = function
    |[] -> failwith "out of bounds : not inside the list"
    |h::t -> 
      if n = e then h 
      else aux (n+1) t
  in aux 1 l;;

let get_cell (x,y) board = 
 let rec aux n = function
    |[] -> failwith "Out of bound : not on the board"
    |h::t -> 
      if n = x then get_element y h
      else aux (n+1) t
  in
  aux 1 board;;

(* OUTIL 12 - put_cell *)

let replace_element value element l = 
  let rec aux n = function
    |[] -> failwith "Out of bounds : not inside the list"
    |h::t -> 
      if n = element then value::t
      else h::aux(n+1) t
  in aux 1 l;;

let put_cell value (x,y) board =
  let rec aux n = function
    |[] -> failwith "Out of bounds : not on the board"
    |e::l -> 
      if n = x then (replace_element value y e)::l
      else e::aux(n+1) l
  in aux 1 board;;
