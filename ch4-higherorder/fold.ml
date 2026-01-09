let rec sum = function
    | [] -> 0
    | h :: t -> h + sum t

let rec concat = function
    | [] -> ""
    | h :: t -> h ^ concat t

let rec sum' init = function
    | [] -> init
    | h :: t -> h + sum' init t

let rec concat' init = function
    | [] -> init
    | h :: t -> h ^ concat' init t

let rec combine op init = function
    | [] -> init
    | h :: t -> h op (combine op init t)

let sum'' = combine ( + ) 0
let concat'' = combine ( ^ ) ""

(* The standard library version of combine *)
let rec combine' f lst acc = 
    match lst with
    | [] -> acc
    | h :: t -> f h (combine' f t acc)

let sum''' lst = combine' ( + ) lst 0
let concat''' lst = combine' ( ^ ) lst ""

(* this is the actual implementation of List.fold_right
    here the acc is just init, but like tail recursion *)
let rec fold_right f lst (acc : 'acc) = match lst with
    | [] -> acc
    | h :: t -> f h (fold_right f t acc)

(* fold_right ( + ) [a; b; c] 0 result in 
    a + (b + (c + 0)) *)


(* here is the tail recursion version *)
let rec combine_tr f acc = function
    | [] -> acc
    | h :: t -> combine_tr f (f h acc) t

let rec combine f acc = function
    | [] -> acc
    | h :: t -> 
        let acc' = combine f acc t in
        f h acc'

let rec combine_tr f acc = function
    | [] -> acc
    | h :: t -> 
        let acc' = f acc h in
        combine_tr f acc' t
    
(* the tail recursion will possibly not working correctly *)
let sub = combine ( - ) 0
(* resulted with 2 *)
let s = sub [3; 2; 1]
let sub_tr = combine_tr ( - ) 0
(* resulted with -6 *)
let s' = sub_tr [3; 2; 1]

(* for combine, it computes 3 - (2 - (1 - 0)) 
   for combine_tr, it computes (((0 - 3) - 2) - 1) *)

(* So the combine_tr is List.flod_left *)
let rec fold_left f (acc : 'acc) = function
    | [] -> acc
    | h :: t -> fold_left f (f acc h) t

(* Using `fold` to implement other functions *)
let length lst = 
    List.fold_right (fun acc _ -> acc + 1) lst 0
(* it's better to use tail recursion *)
let length' lst = 
    List.fold_left (fun acc _ -> acc + 1) 0 lst
let rev lst = 
    List.fold_left (fun acc x -> x :: acc) [] lst

(* fold_right g lst acc = 
    | [] -> []
    | h :: t -> f h (fold_right g t acc) 
    here g is used to apply f to h then cons t *)
let map f lst = 
    List.fold_right (fun x acc -> (f x) :: acc) lst []

let filter f lst = 
    List.fold_right (fun x acc -> if f x then x :: acc else acc) lst []

(* the standard library does not use the fold implementation for list.
    but for more complex data structure, it's more helpful to ensure 
    program correctly and robustly to use fold *)

(* since && have shortage feature, it will immediately return false
    when throughtout first false *)
let rec lst_and_rec = function
    | [] -> true
    | h :: t -> h && lst_and_rec t

(* it will process all the element of given list *)
let lst_and_fold lst = 
    List.fold_left (fun acc elem -> acc && elem) true lst

let lst_and_lib lst = 
    List.for_all (fun x -> x) lst
