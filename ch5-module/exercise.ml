(* Exercise: complex synonym *)

module type ComplexSig = sig
    type t = float * float
    val zero : t
    val add : t -> t -> t
end

(* Exercise: complex encapsulation *)
module Complex : ComplexSig = struct
    type t = float * float
    let zero = (0., 0.)
    let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

(* Remove `zero` from struct : The value zero is required but not provided *)
(* Remove `add` from sig: nothing, bug Complex cannot acccess the add function *)
(* Change 'zero' in struct:  int * int is not included in float * float *)

(* Exercise: big lisit queue *)
module ListQueue = struct
    type 'a t = 'a list
    let empty = []
    let enqueue x q = q @ [x] 
end

let fill_listqueue n = 
    let rec loop n q = 
        if n = 0 then q
        else loop (n - 1) (ListQueue.enqueue n q) in
    loop n ListQueue.empty
(* delayed with 10000 elements.
   delayed of 10 seconds with 30000 elements *)


(* Exercise: big batched queue *)
module  BatchedQueue = struct
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

let fill_batchedqueue n =
    let rec loop n q =
        if n = 0 then q
        else loop (n - 1) (BatchedQueue.enqueue n q) in
    loop n BatchedQueue.empty
(* delayed with 10000000 elements.
   delayed of 10 seconds with 200000000 elements *)

(* Exercise: binary search tree map
    see the directory bstmap *)

(* Exercise: fracation *)
module type FractionSig = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t

  (** [make n d] represents n/d, a fraction with 
      numerator [n] and denominator [d].
      Requires d <> 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t

  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction = struct
    type t = int * int

    let make n d = 
        if d = 0 then failwith "Fraction.make :denominator cannot be 0"
        else (n, d)

    let numerator (n, d) = n
    let denuminator (n, d) = d
    let to_string (n, d) = string_of_int n ^ " / " ^ string_of_int d
    let to_float (n, d) = float_of_int n /. float_of_int d

    let add (n1, d1) (n2, d2) = 
        (n1 * d2 + n2 * d1, d1 * d2)
    
    let mul (n1, d1) (n2, d2) = 
        (n1 * n2, d1 * d2)
end

(* Exercise: fraction reduced *)
module Fraction = struct
    type t = int * int

    let rec gcd x y = 
        if x = 0 then y
        else if x < y then gcd (y - x) x
        else gcd y (x - y)

    let make n d = 
        if d = 0 then failwith "Fraction.make :denominator cannot be 0"
        else let g = gcd n d in (n / g, d / g)

    let numerator (n, d) = n
    let denuminator (n, d) = d
    let to_string (n, d) = string_of_int n ^ " / " ^ string_of_int d
    let to_float (n, d) = float_of_int n /. float_of_int d

    let add (n1, d1) (n2, d2) = 
        let (n, d) = (n1 * d2 + n2 * d1, d1 * d2) in
        let g = gcd n d in (n / g, d / g)
    
    let mul (n1, d1) (n2, d2) = 
        let (n, d) = (n1 * n2, d1 * d2) in
        let g = gcd n d in (n / g, d / g)
end

(* Exercise: make char map *)
module CharMap = Map.Make(Char)
(* val empty : 'a t. here 'a t is a map from key to 'a *)
(* val add : key -> 'a -> 'a t -> 'a t.
    [add key val m] containing all items as same as [m], but
    including (key, val) *)
(* val remove key -> key -> 'a t -> 'a t.
    [remove k map] containing all item as same as [m], except
    the keyword [k] *)

(* Exercise: char ordered *)
(* the module Char has defined the tyep t as char, and
    defined compare as t -> t -> int which compitable with
    the OrderedType sig *)

(* Exercise: use char map *)

let m = CharMap.(empty |> add 'A' "Alpha" |> add 'E' "Echo" 
                |> add 'S' "Sierra" |> add 'V' "Victor")

let _ = CharMap.find 'E' m
let m1 = CharMap.remove 'A' m
let _ = CharMap.mem 'A' m1
let binds = CharMap.bindings m1

(* Exercise: bindings *)
(* since all three map are contain item ('x', 0) ('y', 1),
    they all will return the same association list *)

let is_for m = 
    CharMap.mapi (fun c s -> Printf.sprintf "%c is for %s" c s) m
