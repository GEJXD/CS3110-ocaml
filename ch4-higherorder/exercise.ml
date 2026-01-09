(* Exercise: repeat *)
let rec repeat f n acc = 
    if n > 0
        then repeat f (n - 1) (f acc)
        else acc

(* Exercise: product *)
let rec fold_left f (acc : 'acc) = function
    | [] -> acc
    | h :: t -> fold_left f (f acc h) t

(* the lambda function can replace with ( *. ) *)
let product_left lst = 
    fold_left (fun acc x -> acc *. x) 1.0 lst

let rec fold_right f lst (acc : 'acc) = 
    match lst with
    | [] -> acc
    | h :: t -> f h (fold_right f t acc)

let product_right lst = 
    fold_right (fun x acc -> x *. acc) lst 1.0

(* Exercise: sum_cube_odd *)
let filter f lst = 
    let rec filter_tr f lst acc = 
        match lst with
        | [] -> acc
        | h :: t -> if f h then 
            filter_tr f t (h :: acc) 
            else filter_tr f t acc
    in filter_tr f lst []

let rec map f = function
    | [] -> []
    | h :: t -> (f h) :: map f t

let rec ( -- ) i j = if i > j then [] else i :: (i + 1) -- j

(* Exercise: sum_cube_odd pipeline *)
let sum_cube_odd n  = 
    0 -- n
    |> filter (fun x -> x mod 2 <> 0)
    |> map (fun x -> x * x * x)
    |> fold_left (fun acc x -> acc + x) 0

(* Exercise: exists *)

(* recursion version *)
let rec exists_rec p = function
    | [] -> false
    | h :: t -> p h || (exists_rec p t)

(* tail recursion version *)
let exists_tr p lst = 
    let rec exists_t p lst acc = 
        match lst with
        | [] -> false
        | h :: t -> exists_t p t (acc || p h)
    in exists_t p lst false

(* fold version *)
let exists_fold p lst = 
    fold_left p false lst

(* standard library *)
let exists_lib p lst = 
    List.exists p lst

(* Exercise: account balace *)

let deduct_fold_right balance debits = 
    fold_right (fun x acc -> acc - x) debits balance

let deduct_fold_left balance debits = 
    fold_left (fun acc x -> acc - x) balance debits

let rec deduct_rec balance = function
    | [] -> balance 
    | h :: t -> deduct_rec (balance - h) t

(* Exercise: library uncurried *)
let uncurried_nth (lst, n) = List.nth lst n
let uncurried_append (a, b) = List.append a b
let uncurried_char_compare (a, b) = Char.compare a b
let uncurried_max (a, b) = Stdlib.max a b  

(* Exercise: map composition *)
let call_once f g lst = 
    List.map (fun x -> g x |> f) lst

(* Exercise: more list fun *)
let find_length_gt3 lst = 
    List.filter (fun str -> String.length str > 3) lst
let inc_lst lst = 
    List.map (fun x -> x +. 1.0) lst

let concat_list strs seq = 
    let remove_last_char s = 
        String.sub s 0 (String.length s - 1)
    in List.fold_right (fun x y -> x ^ seq ^ y) strs ""
    |> remove_last_char 

(* Exercise: association list keys *)

(* tail recursion version *)
let keys assoc_list = 
    let add_uniq x acc = 
        if List.mem x acc then acc else x :: acc
    in
    let rec extract acc = function
        | [] -> acc
        | (k, _) :: rest -> extract (add_uniq k acc) rest
    in extract [] assoc_list

(* fold_left version *)
let keys' assoc_list = 
    List.fold_left (fun acc (k, _) ->
        if List.mem k acc then acc else k :: acc)
    []
    assoc_list

(* List.sort_uniq version *)
let keys'' assoc_list = 
    List.map fst assoc_list |> List.sort_uniq compare

(* Exercise: valid matrix *)
let is_valid_matrix = function
    | [] -> false
    | h :: t -> let len = List.length h in
    List.fold_left (fun acc x -> acc && List.length x = len) true t

(* Exercise: row vector add *)
let add_row_vectors xs ys = 
    List.map2 (fun x y -> x + y) xs ys

let rec add_row_vectors' xs ys = 
    match (xs, ys) with
    | ([], []) -> []
    | (x :: xs', y :: ys') -> (x + y) :: add_row_vectors' xs' ys'
    | _ -> invalid_arg "add_row_vectors: the length of 
        xs and ys are not equal"
    
(* Exercise: matrix add *)
let add_matrices xm ym = 
    List.map2 (fun x y -> add_row_vectors x y) xm ym

let rec add_matrices' xm ym = 
    match (xm, ym) with
    | ([[]], [[]]) -> [[]]
    | (x :: xm', y :: ym') -> add_row_vectors x y :: add_matrices' xm' ym'
    | _ -> invalid_arg "add_matrices: the shape of xm and ym are not equal"

(* Exercise: matrix multiply *)
(* I don't understand *)
let transpose ls = 
    let rec transpose' acc = function
        | [] | [] :: _ -> List.rev acc
        | ls -> transpose' (List.map List.hd ls :: acc) (List.map List.tl ls)
    in transpose' [] ls

let dot = List.fold_left2 (fun acc x y -> acc + x * y) 0

let multiply_matrices m1 m2 = 
    List.map (fun row -> List.map (dot row) (transpose m2)) m1
