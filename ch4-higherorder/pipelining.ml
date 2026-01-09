let sum_sq n = 
    let rec loop i sum = 
        if i > n then sum
        else loop (i + 1) (sum + i * i) 
    in loop 0 0

(* or by higher-order programming *)
let from i j = 
    let rec gen i j acc = 
        if i > j then acc else gen (i + 1) j (i :: acc)
    in gen i j [] |> List.rev

let sum = List.fold_left ( + ) 0
(* use pipeline to connect several functions.
    but more space used *)
let sum_sq' n = 
    from 0 n
    |> List.map (fun x -> x * x)
    |> sum

(* Currying *)
let add x y = x + y