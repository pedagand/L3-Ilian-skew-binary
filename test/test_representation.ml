open Numrep.Skew
open Utils
open QCheck

let test (f : 'a -> 'b) (c : 'b Alcotest.testable)
    ((arg, expect, str) : 'a * 'b * string) : unit Alcotest.test_case =
  let result = f arg in
  Alcotest.test_case str `Quick (fun () ->
      Alcotest.(check c) "same result" expect result)

let test_lookup_tree =
  let rec aux i =
    if i = 15 then true else lookup_tree 15 i tree_lookup_test = i && aux (i + 1)
  in
  let result = aux 0 in
  Alcotest.test_case "lookup_tree" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let test_qcheck n name (arbitrary_type : 'a arbitrary) f =
  Test.make ~count:n ~name arbitrary_type f

let () =
  let open Alcotest in
  run "Skew"
    [
      ( "is_canonical",
        List.map (test is_canonical bool)
          [
            ([], true, "[]");
            (s1, true, "(T, 0) :: (O, 3) :: (O, 0) :: []");
            (s3, true, "(O, 0) :: []");
            ([ (T, 1); (T, 1) ], false, "[ (T, 1); (T, 1) ];");
            ([ (O, 1); (T, 1) ], false, "[ (O, 1); (T, 1) ]");
          ] );
      ( "is_well_formed",
        List.map (test is_well_formed bool)
          [
            ( skew_tree1,
              true,
              "[ One (0, tree1); One (1, tree3); One (0, tree4) ]" );
            (skew_tree2, true, "[ Two (0, tree4, tree4); One (2, tree3)  ]");
            ( not_myers,
              false,
              "[ One (0, tree1); One (1, tree3); Two (0, tree4, tree4) ]" );
            ( incorrect_cardinal,
              false,
              "[ One (1, tree1); One (1, tree3); Two (0, tree4, tree4) ]" );
            (lookup_test, true, "lookup_test");
          ] );
      ( "skew_to_int",
        List.map (test skew_to_int int)
          [
            ([], 0, "[]");
            (only_one, pow_2 (50 + 1) - 2 - 50, "Sum 1 * (2^(k+1) - 1)");
            (s1, pow_2 5 + pow_2 6, "mixed");
            (s2, pow_2 12, "mixed");
          ] );
      ( "inc",
        List.map
          (fun x ->
            test
              (fun x -> skew_to_int (inc x))
              int
              (x, skew_to_int x + 1, Format.asprintf "%a" pp_skew x))
          [ s1; s2; s3; s4 ] );
      ( "cons",
        List.map
          (test (cons 1) int_skew_tree)
          [
            ( skew_tree1,
              [
                (1, Two (0, tree1, Leaf 1));
                (7, One (1, tree3));
                (15, One (0, tree4));
              ],
              "[ (1, Two (0, tree1, Leaf 1)); (7, One (1, tree3)); (15, One \
               (0, tree4)) ]" );
            ( skew_tree2,
              [ (15, Two (3, tree4, Node (1, tree3, tree3))) ],
              "[ (15, Two (3, tree4, Node (1, tree3, tree3))) ]" );
          ] );
      ( "head",
        List.map (test head int)
          [
            ( skew_tree1,
              0,
              "[ (1, One (0, tree1)); (7, One (1, tree3)); (15, One (0, \
               tree4)) ]" );
            ( skew_tree2,
              2,
              "[ (7, Two (2, tree3, tree3)); (15, One (0, tree4)) ]" );
          ] );
      ( "tail",
        List.map (test tail int_skew_tree)
          [
            ( skew_tree1,
              [ (7, One (2, tree3)); (15, One (0, tree4)) ],
              "[ (1, One (0, tree1)); (7, One (1, tree3)); (15, One (0, \
               tree4)) ]" );
            ( skew_tree2,
              [
                (3, Two (1, tree2, tree2));
                (7, One (0, tree3));
                (15, One (0, tree4));
              ],
              "[ (7, Two (2, tree3, tree3)); (15, One (0, tree4)) ]" );
          ] );
      ("lookup tree", [ test_lookup_tree ]);
      ( "update_tree",
        List.map
          (test (fun x -> update_tree 100 15 x tree_lookup_test) int_tree)
          [
            ( 0,
              Node
                ( 100,
                  Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7)),
                  Node
                    (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14))
                ),
              "0" );
            ( 6,
              Node
                ( 0,
                  Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 100, Leaf 7)),
                  Node
                    (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14))
                ),
              "6" );
          ] );
      ( "update",
        List.map
          (test (fun x -> update 100 x lookup_test) int_skew_tree)
          [
            ( 0,
              [
                ( 3,
                  Two
                    ( 1,
                      Node (100, Leaf 16, Leaf 17),
                      Node (18, Leaf 19, Leaf 20) ) );
                (15, One (1, tree_lookup_test));
              ],
              "0" );
            ( 4,
              [
                ( 3,
                  Two
                    ( 1,
                      Node (15, Leaf 16, Leaf 17),
                      Node (18, Leaf 100, Leaf 20) ) );
                (15, One (1, tree_lookup_test));
              ],
              "4" );
            ( 6,
              [
                ( 3,
                  Two
                    (1, Node (15, Leaf 16, Leaf 17), Node (18, Leaf 19, Leaf 20))
                );
                ( 15,
                  One
                    ( 1,
                      Node
                        ( 100,
                          Node
                            ( 1,
                              Node (2, Leaf 3, Leaf 4),
                              Node (5, Leaf 6, Leaf 7) ),
                          Node
                            ( 8,
                              Node (9, Leaf 10, Leaf 11),
                              Node (12, Leaf 13, Leaf 14) ) ) ) );
              ],
              "6" );
          ] );
      ( "to_bin",
        List.map (test to_bin skew)
          [
            ([], [], "[]");
            (skew_tree1, [ (O, 0); (O, 1); (O, 0) ], "skew_tree_1");
            (skew_tree2, [ (T, 2); (O, 0) ], "s2");
          ] );
      ( "QCheck : inc",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "inc" arbitrary_skew (fun s ->
                 skew_to_int s + 1 = skew_to_int (inc s)));
        ] );
      ( "QCheck : dec",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "dec" arbitrary_skew (fun s ->
                 assume (skew_to_int s != 0);
                 skew_to_int s - 1 = skew_to_int (dec s)));
        ] );
      ( "QCheck : sub_1",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "sub_1" arbitrary_skew (fun s ->
                 assume (skew_to_int s > 0);
                 skew_to_int s - 1 = skew_to_int (sub_1 s)));
        ] );
      ( "QCheck : sub",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "sub" (QCheck.pair arbitrary_skew arbitrary_skew)
               (fun (s1, s2) ->
                 assume (skew_to_int s1 >= skew_to_int s2);
                 skew_to_int s1 - skew_to_int s2 = skew_to_int (sub s1 s2)));
        ] );
      ( "QCheck : bijection skew_from_int skew_to_int",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "bijection skew_from_int skew_to_int"
               QCheck.small_int (fun n -> skew_to_int (skew_from_int n) = n));
        ] );
      ( "QCheck : skew_from_int returns canonical skew",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "skew_from_int returns canonical skew"
               QCheck.small_int (fun n -> is_canonical (skew_from_int n)));
        ] );
      ( "QCheck : arbitrary_tree well_formed",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "arbitrary_tree_well_formed" arbitrary_skew_tree
               (fun s -> is_well_formed s));
        ] );
      ( "QCheck : bijection from_list to_list",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "bijective from_list to_list"
               (QCheck.list QCheck.int) (fun l -> to_list (from_list l) = l));
        ] );
      ( "QCheck : bijection from_list to_list 2",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "bijective from_list to_list 2" arbitrary_skew_tree
               (fun l -> from_list (to_list l) = l));
        ] );
      ( "QCheck : équivalence ral et list : cons",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "cons" (QCheck.list QCheck.int) (fun l ->
                 let res = cons 100 (from_list l) in
                 List.equal ( = ) (100 :: l) (to_list res)));
        ] );
      ( "QCheck : équivalence ral et list : cons from arbitrary_skew_tree",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "cons" arbitrary_skew_tree (fun l ->
                 let res = cons 100 l in
                 List.equal ( = ) (List.cons 100 (to_list l)) (to_list res)));
        ] );
      ( "QCheck : équivalence ral et list : tail ",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "tail"
               QCheck.(list int)
               (fun l ->
                 assume (List.length l > 0);
                 let res = tail (from_list l) in
                 List.equal ( = ) (List.tl l) (to_list res)));
        ] );
      ( "QCheck : équivalence ral et list : tail from arbitrary_skew_tree",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "tail" arbitrary_skew_tree (fun l ->
                 assume (List.length l > 0);
                 let res = tail l in
                 List.equal ( = ) (List.tl (to_list l)) (to_list res)));
        ] );
      ( "QCheck : équivalence ral et list : head",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "head"
               QCheck.(list int)
               (fun l ->
                 assume (List.length l > 0);
                 head (from_list l) = List.hd l));
        ] );
      ( "QCheck : équivalence ral et list : head from arbitrary_skew_tree",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "head" arbitrary_skew_tree (fun l ->
                 assume (List.length l > 0);
                 head l = List.hd (to_list l)));
        ] );
      ( "QCheck : équivalence ral et list : lookup",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "lookup"
               QCheck.(list int)
               (fun l ->
                 assume (List.length l > 0);
                 let s = from_list l in
                 lookup 0 s = List.nth l 0));
        ] );
      ( "QCheck : équivalence ral et list : lookup from arbitrary_skew_tree",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "lookup" arbitrary_skew_tree (fun l ->
                 assume (List.length l > 0);
                 lookup 0 l = List.nth (to_list l) 0));
        ] );
      ( "QCheck : relation structurelle ral et bin : cons",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "cons" arbitrary_skew_tree (fun s ->
                 let res = cons 100 s in
                 List.equal ( = ) (inc (to_bin s)) (to_bin res)));
        ] );
      ( "QCheck : relation structurelle ral et bin : tail",
        [
          QCheck_alcotest.to_alcotest
            (test_qcheck 100 "tail" arbitrary_skew_tree (fun s ->
                 assume (skew_to_int (to_bin s) > 0);
                 let res = tail s in
                 List.equal ( = ) (dec (to_bin s)) (to_bin res)));
        ] );
    ]
