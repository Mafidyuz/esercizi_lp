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

    let ( ** ) m1 m2  = 
        let rec rpc r m2 = 
            match r, m2 with
            r, M (y::ys) -> List.fold_left (+) 0 (List.map2 (fun a b -> a * b) r y) :: rpc r (M ys)
            | _ -> []
        in let rec ( ** ) m1 m2 = 
            match m1 with
            M (x::xs) ->  rpc x (transpose m2) :: (M xs) ** m2
            | _ -> []
        in M (m1 ** m2);;
    
    let norm m = 
        match transpose m with
        M mt -> 
            let max = (List.map (fun x -> (List.fold_left (+) 0 x)) mt) 
            in List.find (fun x -> (List.for_all (fun y -> x >= y) max ) ) max;; 


    let ( ** ) m1 m2  = 
        let rec rpc r m2 = 
            match r, m2 with
            r, M (y::ys) -> List.fold_left (+) 0 (List.map2 (fun a b -> a * b) r y) :: rpc r (M ys)
            | _ -> []
        in let rec ( ** ) m1 m2 = 
            match m1 with
            M (x::xs) ->  rpc x (transpose m2) :: (M xs) ** m2
            | _ -> []
        in M (m1 ** m2);;

end;;   