(* not the List.map, aka dictionary *)
module type Map = sig
    type ('k, 'v) map 
    val empty : ('k, 'v) map
    val insert : 'k -> 'v -> ('k, 'v) map -> ('k, 'v) map
    val lookup : 'k -> ('k, 'v) map -> 'v
    val bindings : ('k, 'v) map -> ('k * 'v) list
end

module AssocListMap : Map = struct
    (** The lis [(k1, v1); ...; (kv, vn)] binds key [ki] to value [vi].
        If a key appears more than once in the list, it is bound to
        the left-most occurrence in the list. *)
    type ('k, 'v) map = ('k * 'v) list
    let empty = []
    (* constant time for insert *)
    let insert k v m = (k, v) :: m
    (* linear time for lookup *)
    let lookup k m = List.assoc k m
    let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare)
    let bindings m = m |> keys |> List.map (fun k -> (k, lookup k m))
end

(* standard library implements a map using balanced binary
    trees like cpp, so insert and lookup are logarithm time *)

(* to generate a Map on type t, we should define a type that
    have compare function, and pass to Map.Make() *)
module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

(* for more detail, see:
    https://ocaml.org/manual/5.4/api/Map.html
    https://ocaml.org/manual/5.4/api/Map.S.html *)
module type S = sig
  type key
  type 'a t (* the type of maps from type key to type 'a *)
  val empty : 'a t
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
end

(* An example *)
(* primitive type already have the items [t] and [compare] *)
module IntMap = Map.Make(Int)
let m1 = IntMap.(add 1 "one" empty)
let _ = IntMap.find 1 m1
let _ = IntMap.mem 42 m1
let binds = IntMap.bindings m1

(* m2 : float IntMap.t *)
let m2 = IntMap.(add 1 1. empty)
type date = Mon | Tue | Wed | Thu | Fri | Sat | Sun

(* so implementing a compare is the client's responsibility *)

(* Custom Key Types *)
let int_of_date = function
    | Mon -> 1
    | Tue -> 2
    | Wed -> 3
    | Thu -> 4
    | Fri -> 5
    | Sat -> 6
    | Sun -> 7

(* define the Key module *)
module DateKey = struct
  type t = date
  let compare day1 day2 = 
  int_of_date day1 - int_of_date day2
end

module DateMap = Map.Make(DateKey)
