anteslet alkaline_earth_metals = [4;12;20;38;56;88];;

let rec highestNumber = function
  | [] -> None
  | h::tl -> 
    match highestNumber tl with
    | Some n when n > h -> Some n
    | _ -> Some h;;

let rec sort = function
  | [] -> []
  | h::tl -> (sort (List.filter (fun x -> x <= h) tl)) @ [h] @ (sort (List.filter (fun x -> x > h) tl));;

let merge xs ys = sort(xs @ ys);;