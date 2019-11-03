module Goldbach = struct

    let rec is_prime ?(div=2) = function
        n when n = 1 -> false
        | n when n = 2 -> true
        | n when n mod div = 0 -> false
        | n when div >= n/2 -> true
        | n -> is_prime ~div: (div+1) n;;

    let rec first_n_primes ?(acc=0) n =
        match acc with
        | x when x < n && is_prime x -> x :: first_n_primes ~acc:(acc+1) n 
        | x when x < n -> first_n_primes ~acc:(acc+1) n 
        | _ -> [];;
        
    exception SumNotFound

    let rec find_sum ls' ls'' n = 
        match ls', ls'' with
        | (x::xs), (y::ys) when (x+y)=n -> x, y
        | (x::xs), (y::ys) -> find_sum xs ls'' n
        | [], (y::ys) -> find_sum ls'' ys n
        | _ -> raise SumNotFound;;

    let goldbach n = 
        let p = first_n_primes n in
        find_sum p p n;;

    let rec goldbach_list = function
        | (n,m) when n mod 2 = 0 && n <= m -> (goldbach n) :: (goldbach_list ((n+2), m))
        | (n,m) when n <= m -> goldbach_list ((n+1), m)
        | _ -> [];;

end;;