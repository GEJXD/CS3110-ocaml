let rec add1 = function
    | [] -> []
    | h :: t -> (h + 1) :: add1 t

let rec concat_bang = function
    | [] -> []
    | h :: t -> (h ^ "!") :: concat_bang t

let rec add1' = function
    | [] -> []
    | h :: t -> 
        let f = fun x -> x + 1 in
        (f h) :: add1' t
  
let rec concat_bang' = function
    | [] -> []
    | h :: t ->
        let f = fun  x -> x ^ "!" in
        (f h) ::concat_bang' t

  
let rec add1'' f = function
    | [] -> []
    | h :: t -> (f h) :: add1'' f t

let add1''' = add1'' (fun x -> x + 1)

let rec concat_bang'' f = function
    | [] -> []
    | h :: t -> (f h) :: concat_bang'' f t

let concat_bang''' = concat_bang'' (fun x -> x ^ "!")

let rec transform f = function
    | [] -> []
    | h :: t -> (f h) :: transform f t

(* note that the [add1''''] and [concat_bang''''] 
    here are both high-order functions
    it takes a ['a list] as input and returns a ['a list] as output *)
let add1'''' = transform (fun x -> x + 1)
let concat_bang'''' = transform (fun x -> x ^ "!")

(* for more mathematical reasons, we named [transform] 
    as [map] in OCaml *)
let rec map f = function
    | [] -> []
    | h :: t -> (f h) :: map f t

let p x = print_int x; print_newline (); x + 1
(*
let lst = map p [1; 2] will evaluate within a strange order:
map p [1; 2] = p 1 :: (map p [2]) = p 1 :: (p 2 :: map p [])
 = p 1 :: (p 2 :: [])
so p 2 will evaluate earlier than p 1.
*)

(* use a let expression *)
let rec map f = function
    | [] -> []
    | h :: t -> let h' = f h in h' :: map f t

(* the tail recursive at a high cost operation: acc @ [f h]
        @ opeartion is linear-time complexity *)
let rec map_tr f acc = function
    | [] -> acc
    | h :: t -> map_tr f (acc @ [f h]) t

(* The linear-time implemetion use cons operator
    will occur the acc reverse. *)
let rec rev_map_tr f acc = function
    | [] -> acc
    | h :: t -> rev_map_tr f (f h :: acc) t

(* named by List.rev_map in standard libraray *)