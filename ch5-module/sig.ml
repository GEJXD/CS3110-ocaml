module type MATH = sig
    (** [fact n] is [n!]. **)
    val fact : int -> int
end

module Math : MATH = struct
    let rec fact_aux n acc = 
        if n = 0 then acc else fact_aux (n - 1) (n * acc)
    let fact n = fact_aux n 1
    (* or better *)
    let fact' n = 
        let rec fact_tr n acc = 
            if n = 0 then acc else fact_tr (n - 1) (n * acc)
        in fact_tr n 1
end

(* sig can implementation opacity.
    we can access nor the fact' or fact_aux *)
let x = Math.fact 10

module type Stack = sig
    (* do not define the 'a stack *)
    type 'a t
    exception Empty
    val empty : 'a t
    val is_empty : 'a t -> bool
    val push : 'a -> 'a t -> 'a t
    val peek : 'a t -> 'a
    val pop : 'a t -> 'a t
    val size : 'a t -> int
end

(* another kind of inheritance *)
module ListStack : Stack = struct
    type 'a t = 'a list * int
    exception Empty
    let empty = ([], 0)
    let is_empty = function 
        | ([], _) -> true 
        | _ -> false
    let push x (st, s) = (x :: st, s + 1)
    let peek = function
        | ([], _) -> raise Empty
        | (x :: _, _) -> x
    let pop = function
        | ([], _) -> raise Empty
        | (_ :: xs, s) -> (xs, s - 1)
    let size (_, s) = s
end

(* Formatter *)
let kupo_pp fmt s = Format.fprintf fmt "%s kupo" s
let len_pp fmt s = Format.fprintf fmt "%s: length with %d" s (String.length s)


module ListStack' = struct
    type 'a t = 'a list * int
    exception Empty
    let empty = ([], 0)
    let is_empty = function 
        | ([], _) -> true 
        | _ -> false
    let push x (st, s) = (x :: st, s + 1)
    let peek = function
        | ([], _) -> raise Empty
        | (x :: _, _) -> x
    let pop = function
        | ([], _) -> raise Empty
        | (_ :: xs, s) -> (xs, s - 1)
    let size (_, s) = s
    let pp pp_val fmt s =
    let open Format in
    let pp_break fmt () = fprintf fmt "@," in
    fprintf fmt "@[<v 0>top of stack";
    if s <> [] then fprintf fmt "@,";
    pp_print_list ~pp_sep:pp_break pp_val fmt s;
    fprintf fmt "@,bottom of stack@]"
end

