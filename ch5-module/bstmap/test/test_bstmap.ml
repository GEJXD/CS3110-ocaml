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

(* Test helper: collect all bindings via iter *)
let to_list m =
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
  let m1 = IntMap.empty in
  assert (IntMap.is_empty m1);
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

(* ===== Test find raises Not_found ===== *)
let test_find_not_found () =
  let m = IntMap.empty in
  let raised = 
    try
      let _ = IntMap.find 42 m in false
    with Not_found -> true
  in
  assert raised;
  print_endline "✓ test_find_not_found passed"

(* ===== Test remove ===== *)
let test_remove () =
  let m = of_list [(1, "a"); (2, "b"); (3, "c")] in
  let m' = IntMap.remove 2 m in
  assert (IntMap.mem 1 m');
  assert (not (IntMap.mem 2 m'));
  assert (IntMap.mem 3 m');
  print_endline "✓ test_remove passed"

(* ===== Test remove root node ===== *)
let test_remove_root () =
  (* Build a tree where 2 is the root: add 2, then 1, then 3 *)
  let m = IntMap.empty
    |> IntMap.add 2 "two"
    |> IntMap.add 1 "one"
    |> IntMap.add 3 "three"
  in
  let m' = IntMap.remove 2 m in
  assert (not (IntMap.mem 2 m'));
  assert (IntMap.mem 1 m');
  assert (IntMap.mem 3 m');
  print_endline "✓ test_remove_root passed"

(* ===== Test remove non-existent key ===== *)
let test_remove_nonexistent () =
  let m = of_list [(1, "a"); (2, "b")] in
  let m' = IntMap.remove 999 m in
  assert (IntMap.mem 1 m');
  assert (IntMap.mem 2 m');
  print_endline "✓ test_remove_nonexistent passed"

(* ===== Test iter traverses in order ===== *)
let test_iter_order () =
  let m = of_list [(5, "e"); (1, "a"); (3, "c"); (2, "b"); (4, "d")] in
  let bindings = to_list m in
  let keys = List.map fst bindings in
  assert (keys = [1; 2; 3; 4; 5]);
  print_endline "✓ test_iter_order passed"

(* ===== Test with many elements ===== *)
let test_many_elements () =
  let n = 100 in
  let m = ref IntMap.empty in
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
  print_endline "✓ test_many_elements passed"

(* ===== Run all tests ===== *)
let () =
  print_endline "Running Bstmap tests...\n";
  test_empty ();
  test_is_empty ();
  test_add_find ();
  test_add_update ();
  test_mem ();
  test_find_not_found ();
  test_remove ();
  test_remove_root ();
  test_remove_nonexistent ();
  test_iter_order ();
  test_many_elements ();
  print_endline "\n✓ All tests passed!"
