#use "StackADT.mli";;

module PolishCalculator(S : StackADT) = struct

    type expr =  I of int
        | Plus of expr * expr
        | Minus of expr * expr
        | Per of expr * expr
        | Div of expr * expr
        | Pot of expr * expr;;

    exception BadPolishNotation;;

    let is_a_number s = 
        try int_of_string s; true
        with e -> false;; 

    let expr_of_string str = 
        let expressions = S.empty() in 
        let elements = String.split_on_char ' ' str in 
        try
            List.iter (fun s -> 
                match s with
                | s when is_a_number s -> S.push expressions (I (int_of_string s))
                | op -> 
                    match op with
                    | "+" -> S.push expressions (Plus (S.pop expressions , S.pop expressions))
                    | "-" -> S.push expressions (Minus (S.pop expressions , S.pop expressions))
                    | "*" -> S.push expressions (Per (S.pop expressions , S.pop expressions))
                    | "/" -> S.push expressions (Div (S.pop expressions , S.pop expressions))
                    | "**" -> S.push expressions (Pot (S.pop expressions , S.pop expressions))
                    | _ -> raise BadPolishNotation
            ) elements;
            S.pop expressions
        with e -> raise BadPolishNotation;;

    let rec eval e = 
        match e with
        | I n -> n
        | Plus (a, b) -> (eval a) + (eval b)
        | Minus (a, b) -> (eval a) - (eval b)
        | Per (a, b) -> (eval a) * (eval b)
        | Div (a, b) -> (eval a) / (eval b)
        | Pot (b, e) ->  int_of_float ((float_of_int (eval b)) ** (float_of_int (eval e)));;

end;;

#use "UnboundedStack.ml";;
open UnboundedStack;;

module P = PolishCalculator(UnboundedStack);;

let main() = 
    Printf.printf "%d\n" ("3 4 + 5 *" |> P.expr_of_string |> P.eval);; 

main();;

    