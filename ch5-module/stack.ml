(** A persistent stack abstraction.
    
    This module type defines a purely functional (persistent) stack.
    All operations return new stacks; the orginal stack is never modified. *)
module type Stack = sig
    (** The type of stacks containing elemetns of type ['a] *)
    type 'a stack
    (** Raised when [peek] of [pop] is applied to an empty stack *)
    exception Empty
    (** The empty stack *)
    val empty : 'a stack
    (** [is_empty s] return s [true] if and only if [s] 
        contains no elements. *)
    val is_empty : 'a stack -> bool
    (** [push x st] returns a new stack with [x] at the top *)
    val push : 'a -> 'a stack -> 'a stack
    (** [peek st] returns the top element of [s] *)
    val peek : 'a stack -> 'a
    (** [pop st] returns a new stack with the top element removed *)
    val pop : 'a stack -> 'a stack
    (** [size st] returns the number of elements in [s] *)
    val size : 'a stack -> int
    (** [to_list st] returns a list containint all elements of [s],
        ordered from top to bottom *)
    val to_list : 'a stack -> 'a list
end

module ListStack : Stack = struct
    type 'a stack = 'a list * int
    exception Empty
    let empty = ([], 0)
    let is_empty = function
        | ([], _) -> true
        | _ -> false
    let push x (xs, s) = (x :: xs, s + 1)
    let peek = function
        | ([], _) -> raise Empty
        | (x :: xs, _) -> x
    let pop = function
        | ([], _) -> raise Empty
        | (x :: xs, s) -> (xs, s)
    let size (_, s) = s
    let to_list (st, s) = st
end
