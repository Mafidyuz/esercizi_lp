module UnboundedStack = struct
  type 'a stack = {
      mutable c : 'a list
  }
  exception EmptyStackException
  
  let empty() = { c = [] }

  let push s x = s.c <- x :: s.c

  let pop s =
      match s.c with
          hd::tl -> s.c <- tl; hd
          | [] -> raise EmptyStackException

  let top s =
      match s.c with
          hd::_ -> hd
          | [] -> raise EmptyStackException
  
  let is_empty s = (s.c = [])
end;;