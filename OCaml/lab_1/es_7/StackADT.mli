module type StackADT =
  sig
    type 'a stack = { mutable c : 'a list; }
    exception EmptyStackException
    val empty : unit -> 'a stack
    val push : 'a stack -> 'a -> unit
    val pop : 'a stack -> 'a
    val top : 'a stack -> 'a
    val is_empty : 'a stack -> bool
  end;;