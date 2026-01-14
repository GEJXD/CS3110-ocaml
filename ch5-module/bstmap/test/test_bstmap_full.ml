(* Unit tests for Bstmap module *)
open Bstmap

module IntOrder : Bstmap.OrderedType with type t = int = struct
  type t = int
  let compare = Int.compare
end

module IntMap = Bstmap.Make(IntOrder)

(* Test helper: convert list to map *)
let of_list lst =
  List.fold_left (fun m (k, v) -> IntMap.add k v m) IntMap.empty lst

(* Test helper: collect all bindings via iter in traversal order *)
let to_list_in_order m =
  let result = ref [] in
  IntMap.iter (fun k v -> result := (k, v) :: !result) m;
  List.rev !result

(* ===== Test empty ===== *)
let test_empty () =
  assert (IntMap.is_empty IntMap.empty);
  print_endline "✓ test_empty passed"

(* ===== Test is_empty ===== *)
let test_is_empty () =
  let m = IntMap.add 1 "one" IntMap.empty in
  assert (not (IntMap.is_empty m));
  print_endline "✓ test_is_empty passed"

(* ===== Test add and find ===== *)
let test_add_find () =
  let m = IntMap.empty
    |> IntMap.add 1 "one"
    |> IntMap.add 2 "two"
    |> IntMap.add 3 "three"
  in
  assert (IntMap.find 1 m = "one");
  assert (IntMap.find 2 m = "two");
  assert (IntMap.find 3 m = "three");
  print_endline "✓ test_add_find passed"

(* ===== Test add updates existing key ===== *)
let test_add_update () =
  let m = IntMap.empty
    |> IntMap.add 1 "one"
    |> IntMap.add 1 "ONE"
  in
  assert (IntMap.find 1 m = "ONE");
  print_endline "✓ test_add_update passed"

(* ===== Test mem ===== *)
let test_mem () =
  let m = of_list [(1, "a"); (5, "b"); (3, "c")] in
  assert (IntMap.mem 1 m);
  assert (IntMap.mem 5 m);
  assert (IntMap.mem 3 m);
  assert (not (IntMap.mem 0 m));
  assert (not (IntMap.mem 10 m));
  print_endline "✓ test_mem passed"

(* ===== Test find raises Not_found on empty map ===== *)
let test_find_not_found_empty () =
  let raised = 
    try
      let _ = IntMap.find 42 IntMap.empty in false
    with Not_found -> true
  in
  assert raised;
  print_endline "✓ test_find_not_found_empty passed"

(* ===== Test find raises Not_found on non-empty map ===== *)
let test_find_not_found_nonempty () =
  let m = of_list [(1, "a"); (2, "b")] in
  let raised = 
    try
      let _ = IntMap.find 3 m in false
    with Not_found -> true
  in
  assert raised;
  print_endline "✓ test_find_not_found_nonempty passed"

(* ===== Test remove ===== *)
let test_remove () =
  let m = of_list [(1, "a"); (2, "b"); (3, "c")] in
  let m' = IntMap.remove 2 m in
  assert (IntMap.mem 1 m');
  assert (not (IntMap.mem 2 m'));
  assert (IntMap.mem 3 m');
  print_endline "✓ test_remove passed"

(* ===== Test remove root node with two children ===== *)
let test_remove_two_children () =
  (* Build:
        5
       / \
      3   7
     / \ / \
    2  4 6 8
  *)
  let m = of_list [(5, "5"); (3, "3"); (7, "7"); (2, "2"); (4, "4"); (6, "6"); (8, "8")] in
  let m' = IntMap.remove 5 m in
  assert (not (IntMap.mem 5 m'));
  (* After removal, BST should contain [2;3;4;6;7;8] in sorted order *)
  let bindings = to_list_in_order m' in
  let keys = List.map fst bindings in
  assert (keys = [2; 3; 4; 6; 7; 8]);
  (* Also verify values are preserved *)
  assert (List.map snd bindings = ["2"; "3"; "4"; "6"; "7"; "8"]);
  print_endline "✓ test_remove_two_children passed"

(* ===== Test remove root that is leaf or has one child ===== *)
let test_remove_root_simple () =
  let m1 = IntMap.add 1 "x" IntMap.empty in
  let m1' = IntMap.remove 1 m1 in
  assert (IntMap.is_empty m1');

  let m2 = of_list [(2, "2"); (1, "1")] in (* 2 is root, left child only *)
  let m2' = IntMap.remove 2 m2 in
  assert (to_list_in_order m2' = [(1, "1")]);

  let m3 = of_list [(2, "2"); (3, "3")] in (* 2 is root, right child only *)
  let m3' = IntMap.remove 2 m3 in
  assert (to_list_in_order m3' = [(3, "3")]);

  print_endline "✓ test_remove_root_simple passed"

(* ===== Test remove non-existent key ===== *)
let test_remove_nonexistent () =
  let m = of_list [(1, "a"); (2, "b")] in
  let m' = IntMap.remove 999 m in
  assert (IntMap.mem 1 m');
  assert (IntMap.mem 2 m');
  (* Ensure physical equality or at least semantic equality *)
  assert (to_list_in_order m = to_list_in_order m');
  print_endline "✓ test_remove_nonexistent passed"

(* ===== Test remove on empty map ===== *)
let test_remove_empty () =
  let m' = IntMap.remove 42 IntMap.empty in
  assert (IntMap.is_empty m');
  print_endline "✓ test_remove_empty passed"

(* ===== Test iter traverses in strict in-order (not just sorted result) ===== *)
let test_iter_inorder () =
  (* Insert in arbitrary order to build a non-trivial tree *)
  let m = of_list [(5, "e"); (1, "a"); (3, "c"); (2, "b"); (4, "d")] in
  let visited = ref [] in
  IntMap.iter (fun k _ -> visited := k :: !visited) m;
  let order = List.rev !visited in
  assert (order = [1; 2; 3; 4; 5]);
  print_endline "✓ test_iter_inorder passed"

(* ===== Test iter on empty map ===== *)
let test_iter_empty () =
  let count = ref 0 in
  IntMap.iter (fun _ _ -> incr count) IntMap.empty;
  assert (!count = 0);
  print_endline "✓ test_iter_empty passed"

(* ===== Test consistency between mem and find ===== *)
let test_mem_consistency () =
  let m = of_list [(1, "a"); (3, "c"); (5, "e")] in
  for k = 0 to 6 do
    let has_mem = IntMap.mem k m in
    let has_find = 
      try ignore (IntMap.find k m); true with Not_found -> false
    in
    assert (has_mem = has_find)
  done;
  print_endline "✓ test_mem_consistency passed"

(* ===== Test add then immediately remove ===== *)
let test_add_then_remove () =
  let m = IntMap.add 42 "x" IntMap.empty in
  let m' = IntMap.remove 42 m in
  assert (IntMap.is_empty m');
  print_endline "✓ test_add_then_remove passed"

(* ===== Test many elements (including degenerate case) ===== *)
let test_many_elements () =
  let n = 100 in
  let m = ref IntMap.empty in
  (* Insert in increasing order → degenerate tree (linked list) *)
  for i = 1 to n do
    m := IntMap.add i (string_of_int i) !m
  done;
  for i = 1 to n do
    assert (IntMap.find i !m = string_of_int i)
  done;
  (* Remove half *)
  for i = 1 to n / 2 do
    m := IntMap.remove i !m
  done;
  for i = 1 to n / 2 do
    assert (not (IntMap.mem i !m))
  done;
  for i = n / 2 + 1 to n do
    assert (IntMap.mem i !m)
  done;
  (* Verify remaining keys are in order *)
  let remaining_keys = List.map fst (to_list_in_order !m) in
  let expected = List.init (n / 2) (fun i -> n / 2 + 1 + i) in
  assert (remaining_keys = expected);
  print_endline "✓ test_many_elements passed"

(* ===== Run all tests ===== *)
let () =
  print_endline "Running Bstmap tests...\n";
  test_empty ();
  test_is_empty ();
  test_add_find ();
  test_add_update ();
  test_mem ();
  test_find_not_found_empty ();
  test_find_not_found_nonempty ();
  test_remove ();
  test_remove_two_children ();
  test_remove_root_simple ();
  test_remove_nonexistent ();
  test_remove_empty ();
  test_iter_inorder ();
  test_iter_empty ();
  test_mem_consistency ();
  test_add_then_remove ();
  test_many_elements ();
  print_endline "\n✓ All tests passed!"