let map = [("rectangle", 1); ("nonagon", 9); ("icosagon", 20)]

let insert k v lst =
    (k, v) :: lst

let rec lookup k = function
    | [] -> None
    | (k', v) :: t -> if k' = k then Some v else lookup k t

let find k lst = match lookup k lst with
    | None -> raise Not_found
    | Some x -> x

let map = insert "fuck" 1 map