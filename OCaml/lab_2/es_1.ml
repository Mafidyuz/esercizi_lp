module Graph = struct

    type 'a graph = Graph of ('a list) * ( ('a*'a) list ) 

    let empty() = Graph([],[])

end;;