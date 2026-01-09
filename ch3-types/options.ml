let get_val default o = function
    | None -> default
    | Some x -> x

let rec length (lst : 'a list) : int = 
    match lst with
    | [] -> 0
    | h :: t -> 1 + length t

let rec length_tail_rec (lst : 'a list) (acc : int) : int = 
    match lst with
    | [] -> acc
    | h :: t -> length_tail_rec t (acc + 1)

let rec length_tr lst = length_tail_rec lst 0

let rec sum (lst : 'a list) : int = 
    match lst with
    | [] -> 0
    | h :: t -> h + sum t

let rec sum_tail_rec (lst : 'a list) (acc : int) : int = 
    match lst with
    | [] -> acc
    | h :: t -> sum_tail_rec t (acc + h)

let sum_tr lst = sum_tail_rec lst 0

let rec list_max = function
    | [] -> None
    | h :: t -> begin
        match list_max t with
        | None -> Some h
        | Some x -> Some (max x h)
    end