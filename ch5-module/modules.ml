(* module just is some definitions, but scoped with a name *)
module MyModule = struct
    let inc x = x + 1
    type primary_color = Red | Green | Blue
    exception Oops
end

(* this definition are closely relative, such as a data structure *)
module ListStack = struct
    (** [empty] is the empty stack **)
    let empty = []
    (** [is_empty] is whether [s] is empty. **)
    let is_empty = function
        | [] -> true
        | _ -> false

    (** [push x s] pushed [x] onto the top of [s] **)
    let push x s = x :: s

    (** [Empty] is raised when an opeartion cannot be
        applied to am empty stack. **)
    exception Empty

    (** [peek s] is the top element of [s].
        Raises [Empty] if [s] is empty **)
    let peek = function
        | [] -> raise Empty
        | x :: _ -> x

    (** [pop s] is all but the top element of [s].
        Raises [Empty] if [s] is empty **)
    let pop = function
        | [] -> raise Empty
        | _ :: s -> s
end

(* ListStack.(e), all the names from ListStack become usable in e *)
let stack = ListStack.(push 2 (push 1 empty))
(* using pipeline is better to read *)
let stack' = ListStack.(empty |> push 1 |> push 2)

(* the syntax is:
module ModuleName = struct
    module_items
end

module_items can include [let] definitions, [type] definitions,
[exception] definitions, even nested [module] definitions.

module names must begin with an uppercase letter, and idiomatically
use [CamelCase] rather than [snake_case]
*)

(* more accurate version of  the syntax:
    module ModuleName = module_expression
*)
module L = List

(* global open a module: may shadow some function
    like [length] or [map] *)
open List
open String

(* local open a module: *)
let lower_trim s = 
    let open String in
    s |> trim |> lowercase_ascii

(* a signature specified the interface that a module 
    hvae. can understand like the type of a module *)
module type LIST_STACK = sig
    exception Empty
    val empty: 'a list
    val is_empty : 'a list -> bool
    val push: 'a -> 'a list -> 'a list
    val peek : 'a list -> 'a
    val pop: 'a list -> 'a list
end

(* module can be limited by a sig *)
module ListStack' : LIST_STACK = struct
    let empty = []
    let is_empty = function
        | [] -> true
        | _ -> false
    let push x s = x :: s
    exception Empty
    let peek = function
        | [] -> raise Empty
        | x :: xs -> x
    let pop = function
        | [] -> raise Empty
        | x :: xs -> xs
end

(* sig syntax:
    module type ModuleTypeName = module_type *)

(* type annnotations can write with either module 
    name or module expression *)
module ListStackAlias : LIST_STACK = ListStack
module ListStackAlias = (ListStack : LIST_STACK)
module M : sig val x : int end = struct let x = 42 end
module M = (struct let x = 42 end : sig val x : int end)

(* and can include nested module specifications *)
module type X = sig
    val x : int
end

module type T = sig
    module Inner : X
end

module M : T = struct
    module Inner : X = struct
      let x = 42
    end
end
