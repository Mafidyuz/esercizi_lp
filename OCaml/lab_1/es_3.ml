(*A function zeroes to construct a matrix of size n×m filled with zeros.*)
let rec listMake n = 
  match n with
  | 0 -> []
  | n -> 0::(listMake (n-1))

let rec zeroes n m = 
  match m with
  | 0 -> []
  | x -> listMake(n)::(zeroes n (x-1)) ;;


(*A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.*)
let rec id_vect n x = 
  match n with 
  | 0 -> []
  | n when n = (x+1) -> 1::id_vect (n-1) x
  | _ -> 0::id_vect (n-1) x;;

let identity n = 
  let rec identity_aux n x = 
    match x with
    | 0 -> [id_vect n 0]
    | x -> id_vect n x :: identity_aux n (x-1)
  in identity_aux n (n-1);;


(*A function init to construct a square matrix of a given size n filled with the first n×n integers.*) 
let init_vect n =
  let rec init_vect_aux n = 
    match n with
    | 0 -> []
    | n -> n-1::init_vect_aux (n-1)
  in init_vect_aux n |> List.rev;;

let init n = 
  let rec init_aux x = 
    match x with
    | 0 -> []
    | x -> (List.map (fun y -> y + ((x-1)*n)) (init_vect n)) :: init_aux (x-1)
  in init_aux n |> List.rev;;


(*A function transpose that transposes a generic matrix independently of its size and content.*)
let rec get_col m n = 
  match m with
  | [] -> []
  | x::xs -> (List.nth x n) :: (get_col xs n);;

let transpose m = 
  let rec transpose_aux n = 
    match n with
    | n when n = (List.length m) -> []
    | _ -> get_col m n :: transpose_aux (n+1)
  in transpose_aux 0;; 


(*A function * that multiplies two matrices non necessarily square matrices.*)
let rec mult_rc r c = 
  match (r, c) with
   [], _ | _, [] | [], [] -> 0
   | x::xs, y::ys -> x*y + mult_rc xs ys

let rec per m1 m2 = 
  let rec per_aux r m2 = 
    match (r, m2) with
    | r, y::ys -> mult_rc r y :: per_aux r ys
    | _ -> []
  in let rec per_aux1 m1 m2 = 
    match (m1, m2) with
      | x::xs, m2 -> per_aux x m2 :: per_aux1 xs m2
      | _ -> []
  in per_aux1 m1 (transpose m2);;