type 'a tree = 
    | Empty
    | Node of int * 'a tree * 'a tree

(* all we need is throughout the tree, and 
    apply f to each node of given tree *)
let rec map_tree f = function
    | Empty -> Empty
    | Node (v, l, r) -> Node (f v, map_tree f l, map_tree f r)

type 'a mylist = 
    | Nil
    | Cons of 'a * 'a mylist

let rec fold_mylist f acc = function
    | Nil -> acc
    | Cons (h, t) -> f h (fold_mylist f acc t)

(* note that the f here need to have three argument,
    or make more sense, named it as merge *)
let rec fold_tree merge acc = function
    | Empty -> acc
    | Node (v, l, r) -> merge v (fold_tree merge acc l) (fold_tree merge acc r)

let size tr = fold_tree (fun _ lsize rsize -> 1 + lsize + rsize) 0 tr
let depth tr = fold_tree (fun _ ldepth rdepth -> 1 + (max ldepth rdepth)) 0 tr

(* as you can see, using fold to implemention is much earlier than recursion.
    but the @ operator takes a linear-time *)
let preorder tr = fold_tree (fun x lseq rseq -> [x] @ lseq @ rseq) [] tr
let inorder tr = fold_tree (fun x lseq rseq -> lseq @ [x] @ rseq) [] tr
let postorder tr = fold_tree (fun x lseq rseq -> lseq @ rseq @ [x]) [] tr

(* always remember that: the tail recursion
    will make list reverse, like the fold_left function *)
let pre_order tr = 
    let rec walk tr acc = 
        match tr with
        | Empty -> acc
        | Node (v, l, r) -> walk r (walk l (v :: acc))
    in walk tr [] |> List.rev

let in_order tr = 
    let rec walk tr acc = 
        match tr with
        | Empty -> acc
        | Node (v, l, r) -> walk r (v :: (walk l acc))
    in walk tr [] |> List.rev

let post_order tr = 
    let rec walk tr acc = 
        match tr with
        | Empty -> acc
        | Node (v, l, r) -> v :: (walk r (walk l acc))
    in walk tr [] |> List.rev

(* when node does not match the p, then drop out
    the whole subtree *)
let rec filter_tree p = function
    | Empty -> Empty
    | Node (v, l, r) ->
        if p v then Node (v, filter_tree p l, filter_tree p r) else Empty