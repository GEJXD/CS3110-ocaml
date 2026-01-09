let even n = 
    n mod 2 = 0

let rec evens = function
    | [] -> []
    | h :: t -> if even h then h :: evens t else evens t

let odd n = 
    n mod 2 <> 0

let rec odds = function
    | [] -> []
    | h :: t -> if odd h then h :: odds t else odds t

let rec filter p = function
    | [] -> []
    | h :: t -> if p h then h :: filter p t else filter p t

let rec filter p lst = 
    let rec filter_tr p acc = function
    | [] -> acc
    | h :: t -> if p h then filter_tr p (h :: acc) t else filter_tr p acc t
in filter_tr p [] lst |> List.rev
