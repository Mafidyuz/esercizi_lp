module Monoid(S : Set) = struct
    type t = S.t;;

    let add a b = S.add a b;;

    let identity i = S.i;;    
end;;