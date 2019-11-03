module type GraphADT = 
    sig
        type 'a graph = Graph of 'a list * ('a * 'a) list
        val empty : unit -> 'a graph
        val is_empty : 'a graph -> bool
        
    end;;