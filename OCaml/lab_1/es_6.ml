module Sin = struct

    let rec fact n : float =
        match n with
        | 0. -> 1.
        | n -> n *. fact (n -. 1.)

    let rec approx (x : float) (n : int) =
        match n with
        | 0 -> 0.
        | n when n mod 2 = 1 -> ((x ** (2. *. (float n) -. 1.)) /. (fact (2. *. (float n) -. 1.)) +. approx x (n - 1))
        | _ -> (-. (x ** (2. *. (float n) -. 1.)) /. (fact (2. *. (float n) -. 1.)) +. approx x (n - 1))

end;;