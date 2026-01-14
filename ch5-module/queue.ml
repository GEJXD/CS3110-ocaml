module type Queue = sig
    (** An ['a qneue] is a queue whose elements have type ['a] .*)
    type 'a queue

    (** Raised if [front] or [dequeue] is applied to the empty queue *)
    exception Empty
    (** [empty] is the empty queue *)
    val empty : 'a queue
    (** [is_empty q] is whether [q] is empty *)
    val is_empty : 'a queue -> bool
    (** [enqueue x q] is the queue [q] with [x] added to the end *) val enqueue : 'a -> 'a queue -> 'a queue
    (** [front q] is  the element at the front of the queue. 
        Raise [Empty] if [q] is empty. *)
    val front : 'a queue -> 'a
    (** [dequeue q] is the queue containing all the elements of [q]
        except the front of [q].
        Raise [empty] if [q] is empty *)
    val dequeue : 'a queue -> 'a queue
    (** [size q] is the number of elemetns in [q] *)
    val size : 'a queue -> int
    (** [to_list q] is a list containing the elements of [q] in order
        from front to back *)
    val to_list : 'a queue -> 'a list
end

module ListQueue : Queue = struct
    type 'a queue = 'a list * int
    exception Empty
    let empty = ([], 0) 
    let is_empty = function
        | ([], _) -> true
        | _ -> false
    let enqueue x (q, s) = (q @ [x], s + 1)
    let front = function 
        | ([], _) -> raise Empty
        | (x :: _, _) -> x
    let dequeue = function
        | ([], _) -> raise Empty
        | (_ :: q, s) -> (q, s - 1)
    let size (_, s) = s
    let to_list (q, _) = q
end

(* implementing a queue with two stacks *)
module  BatchedQueue : Queue = struct
    (** [{o; i}] represents the queue [o @ List.rev i]. For example,
        [{o - [1; 2]; i = [5; 4; 3]}] represents the queue [1; 2; 3; 4; 5]. 
        where [1] is the front. To avoid ambiguity about emptiness,
        whenever the [o] are empty, the [i] alse must be empty. *)
    type 'a queue = {o : 'a list; i : 'a list}

    exception Empty

    let empty = {o = []; i = []}
    let is_empty = function
        | {o = []} -> true
        | _ -> false
    let enqueue x = function
        | {o = []} -> {o = [x]; i = []}
        | {o; i} -> {o;i = x :: i}
    let front = function
        | {o = []} -> raise Empty
        | {o = h :: _} -> h
    let dequeue = function
        | {o = []} -> raise Empty
        | {o = [_]; i} -> {o = List.rev i; i = []}
        | {o = _ :: t; i} -> {o = t; i}
    let size {o; i} = List.(length o + length i)
    let to_list {o; i} = o @ List.rev i
end

module type OptQueue = sig
    type 'a queue
    val empty : 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a -> 'a queue -> 'a queue
    val front : 'a queue -> 'a option
    val dequeue : 'a queue -> 'a queue option
    val size : 'a queue -> int
    val to_list : 'a queue -> 'a list
end

module BatchedQueueOpt : OptQueue = struct
    (** [{o; i}] represents the queue [o @ List.rev i]. For example,
        [{o - [1; 2]; i = [5; 4; 3]}] represents the queue [1; 2; 3; 4; 5]. 
        where [1] is the front. To avoid ambiguity about emptiness,
        whenever the [o] are empty, the [i] alse must be empty. *)
    type 'a queue = 
        {i : 'a list; o : 'a list; s : int}
    let empty = {i = []; o = []; s = 0}
    let is_empty = function
        | {o = []} -> true
        | _ -> false
    let enqueue x = function
        | {o = []; s} -> {o = [x]; i = []; s = s + 1}
        | {o; i; s} -> {o; i = x :: i; s = s + 1}
    let front = function
        | {o = []} -> None
        | {o = h :: _;}-> Some h
    let dequeue = function
        | {o = []} -> None
        | {o = [_];i; s} -> Some {o = List.rev i; i = []; s = s - 1}
        | {o = _ :: t; i; s} -> Some {o = t; i; s = s - 1}
    let size {i; o; s} = s
    let to_list {o; i;} = o @ List.rev i
end

let ( >>| ) opt f = 
    match opt with
    | None -> None
    | Some x -> Some (f x)

let ( >>= ) opt f = 
    match opt with
    | None -> None
    | Some x -> f x

let q = 
    BatchedQueueOpt.(empty |> enqueue 1 |> dequeue >>| enqueue 2 >>= dequeue)