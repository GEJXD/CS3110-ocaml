module type Set = sig
    (** ['a set] is the type of sets wose elements are of type ['a] *)
    type 'a set
    (** [empty] is the empty set. *)
    val empty : 'a set
    (** [mem x s] is whether [x] is an element of [s] *)
    val mem : 'a -> 'a set -> bool
    (** [add x s] is the set that contains [x] and all the elements of [s] *)
    val add : 'a -> 'a set -> 'a set
    (** [elements s] is a list containing the elements of [s]. No guarantee
        is made about the ordering of that list, but each element is
        guaranteed to be unique. *)
    val elements : 'a set -> 'a list
end

module UniqListSet : Set = struct
    type 'a set = 'a list
    let empty = []
    let mem = List.mem
    let add x s = if mem x s then s else x :: s
    let elements = Fun.id
end