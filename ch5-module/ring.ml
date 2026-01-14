module type Ring = sig
    type t
    val zero : t
    val one : t
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( ~- ) : t -> t (* additive inverse *)
    val to_string : t -> string
end

module IntRing : Ring = struct
    type t = int
    let zero = 0
    let one = 1
    let ( + ) = Stdlib.( + )
    let ( * ) = Stdlib.( * )
    let ( ~- ) = Stdlib. ( ~- )
    let to_string = string_of_int
end

let pp_intring fmt i = 
    Format.fprintf fmt "%s" (IntRing.to_string i)

module FloatRing : Ring = struct
    type t = float
    let zero = 0.
    let one = 1.
    let ( + ) = Stdlib.( +. )
    let ( * ) = Stdlib.( *. )
    let ( ~- ) = Stdlib.( ~-. )
    let to_string = string_of_float
end

let pp_floatring fmt f = 
    Format.fprintf fmt "%s" (FloatRing.to_string f)

(* add a constraint using the with keyword *)
module type INT_RING = Ring with type t = int
module type FLOAT_RING = Ring with type t = float

