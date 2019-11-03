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
    
    let transpose = 
        let rec transpose ?(length=0) = function
            x::xs -> List.map2 (fun a b -> a@b) (List.map (fun y -> [y]) x) (transpose ~length:(List.length x) xs)
            | [] -> Array.make length [] |> Array.to_list
        in function 
             M x -> M(transpose x);;
        

end;;   