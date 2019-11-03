module Graph = struct

    type 'a graph = Graph of ('a list) * ( ('a*'a) list );;

    let empty() = Graph([],[]);;

    let is_empty = function
        | Graph ([], _) -> true
        | _ -> false;;

    let adjacents a = function  
        Graph(_, arcs) -> Map.filter (fun (x,y) -> x = a) arcs;;

end;;