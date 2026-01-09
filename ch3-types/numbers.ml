(* a natural is either a [Zero] or a [Succ](means successo)
    of a natural number *)
type nat = 
    | Zero
    | Succ of nat

let zero = Zero
let one = Succ zero
let two = Succ one
let three = Succ two

let iszero = function
    | Zero -> true
    | Succ _ -> false

let pred = function
    | Zero -> failwith "pred of zero is not defined"
    | Succ x -> x

let rec add n1 n2 = 
    match n1 with
    | Zero -> n2
    | Succ x -> add x (Succ n2)

let rec int_of_nat = function
    | Zero -> 0
    | Succ x -> 1 + int_of_nat x

let rec nat_of_int = function
    | x when x = 0 -> Zero
    | x when x > 0 -> Succ (nat_of_int (x - 1))
    | _ -> failwith "nat could not be a negative"

let rec even = function
    | Zero -> true
    | Succ x -> odd x 
and odd = function
    | Zero -> false
    | Succ x -> even x 