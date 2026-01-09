type intlist = 
    | []
    | (::) of int * intlist

let rec sum lst = 
    match lst with
    | [] -> 0
    | (::) (h, t) -> h + sum t

let rec length lst = 
    match lst with
    | [] -> 0
    | (::) (h, t) -> 1 + sum t

let insert v lst = v :: lst

let rec list_max lst = 
    match lst with
    | [] -> None
    | (::) (h, t) -> begin
        match list_max t with
        | None -> Some h
        | Some x -> Some (max x h)
    end

let lst3 = 3 :: []
let lst123 = 1 :: 2 :: lst3

type node = {value : int; next : mylist}
and mylist = Nil | Node of node

type node = {value : int; next : node}