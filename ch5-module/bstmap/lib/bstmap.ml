module type OrderedType = sig
    type t
    val compare : t -> t -> int
    (** [compare x y] returns:
        a negative number when x < y
        0 when x = y
        a positive number when x > y *)
end

module type BstMapSig = sig
    type key
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
end

module Make (C : OrderedType) : (BstMapSig with type key = C.t) = struct
    type key = C.t
    type 'a tree = 
        | Empty
        | Node of 'a tree * key * 'a * 'a tree
    type 'a t = 'a tree

    let empty = Empty
    let is_empty = function
        | Empty -> true
        | _ -> false
    
    let rec mem k = function
        | Empty -> false
        | Node (l, k', _, r) ->
            let cmp = C.compare k k' in
            if cmp = 0 then true
            else if cmp < 0 then mem k l
            else mem k r
    
    let rec add k v = function
        | Empty -> Node (Empty, k, v, Empty)
        | Node (l, k', v', r) -> 
            let cmp = C.compare k k' in
            if cmp = 0 then Node (l, k, v, r) (* update the k v *)
            else if cmp < 0 then Node (add k v l, k', v', r)   
            (* k less than k', modify the left child *)
            else Node (l, k', v', add k v r)
    
    let rec find k = function
        | Empty -> raise Not_found
        | Node (l, k', v', r) ->
            let cmp = C.compare k k' in
            if cmp = 0 then v'
            else if cmp < 0 then find k l
            else find k r
        
    let rec iter f = function
        | Empty -> ()
        | Node (l, k, v, r) -> 
        begin
            iter f l;
            f k v;
            iter f r
        end

    (* [remove_min bst] deletes the minimum node of [bst],
        return [k_min, v_min, new subtree] *)
    let rec remove_min = function
        | Empty -> failwith "remove on empty tree"
        (* left subtree are empty, so current node is the minium node *)
        | Node (Empty, k', v', r) -> (k', v', r)
        (* otherwise, recursively search the left subtree,
            and modify new left subtree *)
        | Node (l, k', v', r) -> 
            let (k_min, v_min, l') = remove_min l in (k_min, v_min, Node (l', k', v', r))
    
    (* remove have three conditions.
        see https://en.wikipedia.org/wiki/Binary_search_tree#Deletion *)
    let rec remove k = function
        | Empty -> Empty
        | Node (l, k', v', r) -> 
            let cmp = C.compare k k' in
            if cmp < 0 then Node (remove k l, k', v', r)
            else if cmp > 0 then Node (l, k', v', remove k r)
            else match l, r with (* k = k' *)
                | Empty, _ -> r
                | _, Empty -> l
                | _, _ -> 
                    let (k_min, v_min, r') = remove_min r in Node (l, k_min, v_min, r')
end