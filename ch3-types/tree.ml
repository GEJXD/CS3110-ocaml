type 'a tree = 
    | Leaf
    | Node of 'a * 'a tree * 'a tree

let rec size = function
    | Leaf -> 0
    | Node (_, l, r) -> 1 + size l + size r

let t = Node (4, 
            Node (2,
                Node (1, Leaf, Leaf),
                Node (3, Leaf, Leaf)
                ),
            Node (5, 
                Node (6, Leaf, Leaf),
                Node (7, Leaf, Leaf)))

let rec size tr = 
    match tr with
    | Leaf -> 0
    | Node (_, l, r) -> 1 + size l + size r

let rec sum tr = 
    match tr with
    | Leaf -> 0
    | Node (u, l, r) -> u + sum l + sum r

let pre_order tr = 
    let rec walk acc tr = 
        match tr with
        | Leaf -> acc
        | Node (u, l, r) -> walk (walk (u :: acc) l) r 
    in
    List.rev (walk [] tr)

let in_order tr = 
    let rec walk acc tr = 
        match tr with
        | Leaf -> acc
        | Node (u, l, r) -> walk (u :: (walk acc l)) r
    in
    List.rev (walk [] tr)

let post_order tr = 
    let rec walk acc tr = 
        match tr with
        | Leaf -> acc
        | Node (u, l, r) -> u :: (walk (walk acc l) r)
    in
    List.rev (walk [] tr)

type 'a tree = 
    | Leaf
    | Node of 'a node
and 'a node = {
    value : 'a;
    left : 'a tree;
    right : 'a tree;
}

let t2 = 
    Node {
        value = 2;
        left = Node {value = 1; left = Leaf; right = Leaf};
        right = Node {value = 3; left = Leaf; right = Leaf}
    }

(*[mem x t] is whether [x] is a value at some node in tree [t]. *)
let rec mem x = function
    | Leaf -> false
    | Node {value; left; right} -> value = x || mem x left || mem x right

let rec preorder = function
    | Leaf -> []
    | Node {value; left; right} -> [value] @ preorder left @ preorder right


let rec (@) l1 l2 = 
    match l1 with
    | [] -> l2
    | h :: t -> h :: (t @ l2)
