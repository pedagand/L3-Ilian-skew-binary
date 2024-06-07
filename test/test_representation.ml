open Numrep.Skew
open Utils

let test_skew_to_int_1 =
  let result = skew_to_int [] in
  let desired = 0 in
  Alcotest.test_case "[]" `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let test_is_canonical_1 =
  let result = is_canonical [] in
  Alcotest.test_case "[]" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let test_is_canonical_2 =
  let result = is_canonical s1 in
  Alcotest.test_case "(T, 0) :: (O, 3) :: (O, 0) :: []" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let test_is_canonical_3 =
  let result = is_canonical s3 in
  Alcotest.test_case "(O, 0) :: []" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let test_is_canonical_4 =
  let result = is_canonical [ (T, 1); (T, 1) ] in
  Alcotest.test_case "(T, 1) :: (T, 1) :: []" `Quick (fun () ->
      Alcotest.(check bool) "same result" false result)

let test_is_canonical_5 =
  let result = is_canonical [ (O, 1); (T, 1) ] in
  Alcotest.test_case "(O, 1) :: (T, 1) :: []" `Quick (fun () ->
      Alcotest.(check bool) "same result" false result)

let test_is_well_formed =
  let result = is_well_formed skew_tree1 in
  Alcotest.test_case "[ One (0, tree1); One (1, tree3); One (0, tree4) ]" `Quick
    (fun () -> Alcotest.(check bool) "same result" true result)

let test_is_well_formed2 =
  let result = is_well_formed skew_tree2 in
  Alcotest.test_case "[ Two (0, tree4, tree4); One (2, tree3)  ]" `Quick
    (fun () -> Alcotest.(check bool) "same result" true result)

let test_is_well_formed3 =
  let result = is_well_formed not_myers in
  Alcotest.test_case "[ One (0, tree1); One (1, tree3); Two (0, tree4, tree4) ]"
    `Quick (fun () -> Alcotest.(check bool) "same result" false result)

let test_is_well_formed4 =
  let result = is_well_formed incorrect_cardinal in
  Alcotest.test_case "[ One (1, tree1); One (1, tree3); Two (0, tree4, tree4) ]"
    `Quick (fun () -> Alcotest.(check bool) "same result" false result)

(* Sum 2^(k+1) - 1 for k = 1 : n equals 2^(n + 1) - 1 - n*)
let test_skew_to_int_2 =
  let result = skew_to_int only_one in
  let desired = pow_2 (50 + 1) - 2 - 50 in
  Alcotest.test_case "Sum 1 * (2^(k+1) - 1)" `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let test_skew_to_int_3 =
  let result = skew_to_int s1 in
  let desired = pow_2 5 + pow_2 6 in
  Alcotest.test_case "mixed" `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let test_skew_to_int_4 =
  let result = skew_to_int s2 in
  let desired = pow_2 12 in
  Alcotest.test_case "mixed" `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let test_inc s =
  let result = skew_to_int (inc s) in
  let desired = skew_to_int s + 1 in
  Alcotest.test_case (Format.asprintf "%a" pp_skew s) `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let test_cons1 =
  let result = cons 1 skew_tree1 in
  let desired : int skew_tree =
    [ (1, Two (0, tree1, Leaf 1)); (7, One (1, tree3)); (15, One (0, tree4)) ]
  in
  Alcotest.test_case "[ One (0, tree1); One (1, tree3); One (0, tree4) ]" `Quick
    (fun () -> Alcotest.(check bool) "same result" true (equal result desired))

let test_cons2 =
  let result = cons 1 skew_tree2 in
  let desired : int skew_tree =
    [ (15, Two (3, tree4, Node (1, tree3, tree3))) ]
  in
  Alcotest.test_case "[ Two (2, tree3, tree3); One (0, tree4) ]" `Quick
    (fun () -> Alcotest.(check bool) "same result" true (equal result desired))

let test_cons3 =
  let result = cons 1 skew_tree2 in
  let desired : int skew_tree =
    [ (15, Two (4, Node (1, tree3, tree3), tree4)) ]
  in
  Alcotest.test_case "[ Two (2, tree3, tree3); One (0, tree4) ]" `Quick
    (fun () -> Alcotest.(check bool) "same result" false (equal result desired))

let generator_small_int =
  let open QCheck in
  Gen.small_int

let generator_skew : skew QCheck.Gen.t =
 fun st ->
  let n = generator_small_int st in
  let rec gen_list acc =
    if acc = 0 then [] else (O, generator_small_int st) :: gen_list (acc - 1)
  in
  let first =
    if n mod 2 = 0 then (O, generator_small_int st)
    else (T, generator_small_int st)
  in
  first :: gen_list n

let arbitrary_skew =
  QCheck.make ~print:(Format.asprintf "%a" pp_skew) generator_skew

let test_inc_q =
  let open QCheck in
  Test.make ~count:100 ~name:"inc" arbitrary_skew (fun s ->
      skew_to_int s + 1 = skew_to_int (inc s))

let test_dec_q =
  let open QCheck in
  Test.make ~count:100 ~name:"dec" arbitrary_skew (fun s ->
      print_string ("s : " ^ Format.asprintf "%a" pp_skew s);
      print_newline ();
      print_string ("dec s : " ^ Format.asprintf "%a" pp_skew (dec s));
      print_newline ();

      skew_to_int s - 1 = skew_to_int (dec s))

let test_head1 =
  let result = head skew_tree1 in
  let desired = 0 in
  Alcotest.test_case
    "[ (1, One (0, tree1)); (7, One (1, tree3)); (15, One (0, tree4)) ]" `Quick
    (fun () -> Alcotest.(check int) "same result" result desired)

let test_head2 =
  let result = head skew_tree2 in
  let desired = 2 in
  Alcotest.test_case "[ (7, Two (2, tree3, tree3)); (15, One (0, tree4)) ]"
    `Quick (fun () -> Alcotest.(check int) "same result" result desired)

let test_tail1 =
  let result = tail skew_tree1 in
  let desired = [ (7, One (1, tree3)); (15, One (0, tree4)) ] in
  Alcotest.test_case
    "[ (1, One (0, tree1)); (7, One (1, tree3)); (15, One (0, tree4)) ]" `Quick
    (fun () -> Alcotest.(check bool) "same result" true (equal result desired))

let test_tail2 =
  let result = tail skew_tree2 in
  let desired =
    [ (3, Two (1, tree2, tree2)); (7, One (0, tree3)); (15, One (0, tree4)) ]
  in
  Alcotest.test_case "[ (7, Two (2, tree3, tree3)); (15, One (0, tree4)) ]"
    `Quick (fun () ->
      Alcotest.(check bool) "same result" true (equal result desired))

let test_lookup_tree =
  let rec aux i =
    if i = 15 then true else lookup_tree 15 i tree_lookup = i && aux (i + 1)
  in
  let result = aux 0 in
  Alcotest.test_case "lookup tree" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let () =
  let open Alcotest in
  run "Skew"
    [
      ( "is_canonical",
        [
          test_is_canonical_1;
          test_is_canonical_2;
          test_is_canonical_3;
          test_is_canonical_4;
          test_is_canonical_5;
        ] );
      ( "is_well_formed",
        [
          test_is_well_formed;
          test_is_well_formed2;
          test_is_well_formed3;
          test_is_well_formed4;
        ] );
      ( "skew_to_int",
        [
          test_skew_to_int_1;
          test_skew_to_int_2;
          test_skew_to_int_3;
          test_skew_to_int_4;
        ] );
      ("inc", [ test_inc s1; test_inc s2; test_inc s3; test_inc s4 ]);
      ("inc (QCheck)", [ QCheck_alcotest.to_alcotest test_inc_q ]);
      ("dec (QCheck)", [ QCheck_alcotest.to_alcotest test_dec_q ]);
      ("cons", [ test_cons1; test_cons2; test_cons3 ]);
      ("head", [ test_head1; test_head2 ]);
      ("tail", [ test_tail1; test_tail2 ]);
      ("lookup tree", [ test_lookup_tree ]);
    ]
