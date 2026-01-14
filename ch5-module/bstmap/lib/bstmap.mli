module type OrderedType = sig
    type t
    val compare : t -> t -> int
    (** [compare x y] returns:
        a negative number when x < y
        0 when x = y
        a positive number when x > y *)
end

module type BstMapSig = sig
    type key

    (** the type of maps from key to type 'a *)
    type 'a t

    (** [empty] returns a empty map *)
    val empty : 'a t

    (** [is_empty m] returns [true] if [m] is empty *)
    val is_empty : 'a t -> bool

    (** [mem x m] returns [true] is [m] contains a binding for [x], 
        and [false] otherwise *)
    val mem : key -> 'a t -> bool

    (** [add key data m] returns a map containing the same binding as m,
        plus a binding of [key] to [data] *)
    val add : key -> 'a -> 'a t -> 'a t
    
    (** [find x m] returns the current value of [x] in [m].
        raises [Not_found] if no binding for [x] exists *)
    val find : key -> 'a t -> 'a

    (** [remove x m] returns a map conaining the same binding as [m], 
        except for [x] which is unbound in the returned map *)
    val remove : key -> 'a t -> 'a t

    (** [iter f m] applies [f] to all bindings in map m. 
        [f] receives the key as first argument, and the associated value as second argument *)
    val iter : (key -> 'a -> unit) -> 'a t -> unit
end

(** [Make] creates a new BstMap module from an OrderedType *)
module Make (C : OrderedType) : BstMapSig with type key = C.t