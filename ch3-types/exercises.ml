(* Exercise: list expressions *)
let ex1 = [1; 2; 3; 4; 5] let ex2 = 1 :: 2 :: 3 :: 4 :: 5 :: []
let ex3 = [1] @ [2; 3; 4] @ [5]

(* Exercise: product
    calculate the product of all the elements in a int list*)
let rec product_tail acc = function
    | [] -> 1
    | h :: t -> product_tail (acc * h) t

let rec product lst = product_tail 1 lst

(* Exercise: concat
    concatenation of all the strings in a string list *)
let rec concat = function
    | [] -> ""
    | h :: t -> h ^ (concat t)

(* Exercise: patterns *)

(* function 1: pattern the first element is "bigred" *)
let first_with_bigred = function
    | [] -> false
    | h :: _ -> if h = "bigred" then true else false

(* function 2 : pattern a list 
    has exactly two or four elements*)
let length_of_two_or_four = function
    | h :: i :: [] -> true
    | h :: i :: j :: k :: [] -> true
    | _ -> false

(* function 3 : first two elements of the list are equal *)
let first_two_equal = function
    | h :: i :: _ -> if h = i then true else false
    | _ -> false

(* Exercise: library *)

(* List.nth and Lst.length *)
let fifth_element (lst : int list) = 
    let len = List.length lst in
    if len < 5 then 0
    else List.nth lst 4

(* The pattern match version *)
let fifth_element (lst : int list) = 
    let len = List.length lst in
    match len with
    | len when len < 5 -> 0
    | _ -> List.nth lst 4

(* sorted in descending order *)
let sort_dec lst =
    List.sort Stdlib.compare lst |> List.rev

(* Exercise: liibrary puzzle *)

(* return last element -- do not consider Nil *)
let last lst = List.rev lst |> List.hd

(* determine whether exist any zero *)
let any_zeros (lst : int list) = 
    let res = List.find_opt (fun x -> x = 0) lst in
    if res = None then false else true

(* Exercise: take drop *)

(* return the first [n] elements of [lst]
    if [lst] has fewer than [n] elements, return all of them *)
let rec take n lst = 
    match lst with
    | [] -> []
    | h :: t -> 
        if n > 0 then h :: take (n - 1) t
        else [] 


(* drop the first [n] elements of [lst]
    if [lst] has fewer than [n] elemetns, return [] *)
let rec drop n lst = 
    match lst with
    | [] -> []
    | h :: t -> 
        if n > 0 then drop (n - 1) t
        else h :: t

(* tail recursive version *)
let rec take_fast n lst acc = 
    match lst with
    | [] -> acc
    | h :: t -> 
        if n > 0 then take_fast (n - 1) t (h :: acc)
        else acc

let take_tr n lst = take_fast n lst [] |> List.rev

(* the drop function already is the tail recursive, 
    no need to rewrite *)

(* Exercise: unimodal 
    return whether that list is unimodal.*)
let is_unimodal lst = 
    let rec is_increase = function
    | [] | [_] -> true
    | x :: y :: rest -> 
        if x <= y then is_increase (y :: rest)
        else is_decrease (y :: rest)
    and is_decrease = function
    | [] | [_] ->  true
    | x :: y :: rest ->
        if x >= y then is_decrease (y :: rest)
        else failwith "return false"
    in is_increase lst

(* Exercise: powerset
    return the all subset of given list *)

(* cons [x :: item] with all item in lst, 
    then append all of them into front of res *)
let rec combine_fast x lst res = 
    match lst with
    | [] -> res 
    | h :: t -> combine_fast x t ((x :: h) :: res)

let rec combine_tr x lst = combine_fast x lst lst

(* powerset [2; 3] = [[]; [2]; [2; 3]; [3]]
   powerset [1; 2; 3] = [[1]; [1; 2]; [1; 2; 3]; [1; 3]] :: powerset [2; 3]
   it just append 1 to all elements of powerset [2; 3]
   particularly, the powerset [] = [[]] *)
let rec powerset lst = 
    match lst with
    | [] -> [[]]
    | h :: t -> let p = powerset t in combine_tr h p

(* Exercise: print int list rec
    prints its input list one number per line. *)
let rec print_int_list = function
    | [] -> ()
    | h :: t -> begin
        print_endline (string_of_int h);
        print_int_list t
    end

(* Exercise: print int list iter
    use iter to throughout list *)
let print_int_list' lst = 
    List.iter (fun x -> print_endline (string_of_int x)) lst

(* Exercise: student *)
type student = {first_name : string; last_name : string; gpa : float}

(* expression 1: have type student *)
let stu = {first_name = "Donald"; last_name = "Trump"; gpa = 114.514}

(* expression 2: have type string * string *)
let name = function
    | {first_name; last_name; _} -> (first_name, last_name)
 
(* expression 3 : have type string->string->float->student *)
let create_stu fst_name lst_name gpa = 
    {first_name = fst_name; last_name = lst_name; gpa = gpa}

(* Exercise: pokerecord *)
type poketype = Normal | Fire | Water
type pokemon = {name : string; hp : int; ptype : poketype}
let foo = {name = "charizard"; hp = 78; ptype = Fire}
let boo = {name = "squirtle"; hp = 44; ptype = Water}

(* Exercise: safe hd nad tl *)

(* [safe_hd lst] return the first element of [lst]
    if [lst] is nil, return [None] *)
let safe_hd = function
    | [] -> None
    | h :: t -> Some h

(* [safe_tl lst] return the last element of [lst] 
    if [lst] is nil, return [None] *)
let rec safe_tl = function
    | [] -> None
    | [x] -> Some x
    | h :: t -> safe_tl t

(* Exercise: pokefun 
    return the pokemon with the highest hp *)
let rec max_hp = function
    | [] -> None
    | h :: t -> begin
         match max_hp t with
         | None -> Some h 
         | Some x -> Some (if x.hp >= h.hp then x else h)
    end

(* Exercise date before
    [is_before d1 d2] returns true if [d1] if before then [d2] *)
type date = int * int * int
let is_before (y1, m1, d1) (y2, m2, d2) = 
    (y1 < y2) || (y1 = y2) && (m1 < m2) || (y1 = y2) && (m1 = m2) && (d1 < d2)

(* Exercise: earliest date 
    [earliest lst] returns the earliest date in the lst *)
let rec earliest = function
    | [] -> None
    | h :: t -> begin
        match earliest t with
        | None -> Some h
        | Some x -> Some (if is_before x h then x else h)
    end

(* Exercise: assoc list *)
let insert k v lst =
    (k, v) :: lst

let rec lookup k = function
    | [] -> raise Not_found
    | (k', v) :: t -> if k' = k then v else lookup k t

(* Exercise cards *)
type suit = Spades | Hearts | Diamonds | Clubs
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King | Ace
type card = {suit : suit; rank : rank}

let ca = {suit = Clubs; rank = Ace}
let hq = {suit = Hearts; rank = Queen}
let dw = {suit = Diamonds; rank = Two}
let ss = {suit = Spades;rank = Seven}

(* Exercise: matching *)

(* for pattern Some x :: tl, the [None] does not match *)
(* for pattern [Some 3110; None] , [None] does not match *)
(* for pattern [Some x; _;], [None] does not match  *)
(* for pattern [h1 :: h2 :: tl], [None] does not match *)
(* for pattern h :: tl,  only [] does not match *)

(* Exercise: quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x : int) : sign =
    if x < 0 then Neg
    else if x = 0 then Zero
    else Pos

let quadrant : int * int -> quad option = fun (x, y) ->
    match (sign x, sign y) with
    | (Pos, Pos) -> Some I
    | (Neg, Pos) -> Some II
    | (Neg, Neg) -> Some III
    | (Pos, Neg) -> Some IV
    | (_, _) -> None

(* Exercise quadrant when *)
let quadrant_when : int * int -> quad option = fun (x, y) ->
    match (x, y) with
    | (x, y) when x > 0 && y > 0 -> Some I
    | (x, y) when x < 0 && y > 0 -> Some II
    | (x, y) when x < 0 && y < 0 -> Some III
    | (x, y) when x > 0 && y < 0 -> Some IV 
    | (_, _) -> None

(* Exercise: depth *)
type 'a tree = | Empty | Node of 'a * 'a tree * 'a tree

let rec depth_tr tr acc = 
    match tr with
    | Empty -> acc
    | Node (_, l, r) -> 1 + max (depth_tr l acc) (depth_tr r acc)

let depth tr = depth_tr tr 0

(* testcase, should return 3 *)
let t = Node (1, 
              Node (2, Empty, Empty),
              Node (3, 
                    Node (4, Empty, Empty), 
                    Empty))

(* Exercise : shape *)
let rec same_shape ta tb =
    match (ta, tb) with
    | (Empty, Empty) -> true
    | (Node (_, la, ra), Node (_, lb, rb)) ->
        same_shape la lb && same_shape ra rb
    | _ -> false

(* Some testcases here 
let leaf = Empty

let t1 = Node (1, Empty, Empty)

let t2 = Node (1, 
               Node (2, Empty, Empty),
               Empty)

let t3 = Node (1, 
               Empty,
               Node (2, Empty, Empty))

let t4 = Node (1,
               Node (2, Empty, Empty),
               Node (3, Empty, Empty))

let t5 = Node (1,
               Node (2,
                     Node (3, Empty, Empty),
                     Empty),
               Empty)

let t6 = Node ("a",
               Node ("b", Empty, Empty),
               Node ("c", Empty, Empty))

let t7 = Node (1,
               Node (2, Empty, Empty),
               Node (3,
                     Node (4, Empty, Empty),
                     Empty))

let t8 = Node (1,
               Node (2,
                     Empty,
                     Node (3, Empty, Empty)),
               Empty)
*)

(* should return:
    same_shape leaf leaf -> true
    same_shape t1 t1 -> true
    same_shape t1 leaf -> false
    same_shape t2 t3 -> false
    same_shape t2 t2 -> true
    same_shape t4 t6 -> true
    same_shape t5 t8 -> false
    same_shape t4 t7 -> false
    same_shape t5 t5 -> true
    same_shape leaf t1 -> false *)

(* Exercise: list max exn *)
let rec list_max = function
    | [] -> raise (Failure "empty")
    | [x] -> x
    | h :: t -> max h (list_max t)

(* Exercise : list max exn string*)
let rec list_max_opt = function
       | [] -> None
       | h :: t -> begin
            match list_max_opt t with
            | None -> Some h
            | Some x -> Some (max x h)
       end

let list_max_string lst = 
    match list_max_opt lst with
    | None -> "empty"
    | Some x -> string_of_int x

(* Exercise: is_bst *)
(* in_order of a binary search tree is increasing *)
let is_bst tr = 
    let rec check min_opt max_opt = function
    | Empty -> true
    | Node (v, l, r) -> 
        (match min_opt with None -> true | Some lo -> v > lo) &&
        (match max_opt with None -> true | Some hi -> v < hi) &&
        (* the left subtree always less than v *)
        check min_opt (Some v) l &&
        (* the right subtree always greater than v *)
        check (Some v) max_opt r
    in check None None tr

(* Some testcases here
let t1 = Node (10, 
               Node (5, Empty, Empty), 
               Node (15, Empty, Empty))

let t2 = Node (10,
               Node (5, Empty, Node (15, Empty, Empty)),
               Empty)

let t3 = Node (1, Empty, Empty)

let t4 = Empty
*)

(* should return:
    is_bst t1 -> true
    is_bst t2 -> false
    is_bst t3 -> true
    is_bst t4 -> true
*)
