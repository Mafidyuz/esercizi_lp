module Matrix = struct
    type matrix = M of (int list) list ;;

    let (==) a b = 
        match a, b with
        (M x, M y) -> x = y;;

    let (++) a b = 
        match a, b with
        (M x, M y) -> M (List.map2 (fun x' y' -> (List.map2 (fun x'' y'' -> x'' + y'') x' y') ) x y);;

    let ( **. ) a k =
        match a with
        M x -> M (List.map (fun y -> (List.map (fun z -> z * k)) y ) x );;

    let make_column a = 
        match a with
        M (x::xs) -> List.map (fun y -> y ) x 

    let rec transpose a = 
        match a with
        M a -> match a with
            | x::xs -> List.map ( fun y -> y:: ) x

end;;   