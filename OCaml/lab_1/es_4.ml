let count text word =
  let rec count_aux text acc n nw =
  match n with
  | n when n = String.length text -> acc
  | _ -> 
    match nw with
    | nw when nw = String.length word -> count_aux text (acc+1) (n+1) 0 
    | nw when text.[n] = word.[nw] -> count_aux text acc (n+1) (nw+1)
    | _ -> count_aux text acc (n+1) 0
  in count_aux (text^".") 0 0 0;;