open Numrep.Skew
open Utils

let test ((arg, expect, str) : 'a * 'b * string) (f : 'a -> 'b)
    (c : 'b Alcotest.testable) : unit Alcotest.test_case =
  let result = f arg in
  Alcotest.test_case str `Quick (fun () ->
      Alcotest.(check c) "same result" expect result)

let test_2 ((arg, desired, expect, str) : 'a * 'b * bool * string)
    (f : 'a -> 'b) (eq : 'b -> 'b -> bool) : unit Alcotest.test_case =
  let result = f arg in
  Alcotest.test_case str `Quick (fun () ->
      Alcotest.(check bool) "same result" expect (eq desired result))

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
      skew_to_int s - 1 = skew_to_int (dec s))

let test_lookup_tree =
  let rec aux i =
    if i = 15 then true else lookup_tree 15 i tree_lookup_test = i && aux (i + 1)
  in
  let result = aux 0 in
  Alcotest.test_case "lookup_tree" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let test_lookup =
  let rec aux i =
    if i = 10 then true
    else
      (let desired = if i < 6 then i + 15 else i - 6 in
       lookup i lookup_test = desired)
      && aux (i + 1)
  in
  let result = aux 0 in
  Alcotest.test_case "lookup_tree" `Quick (fun () ->
      Alcotest.(check bool) "same result" true result)

let () =
  let open Alcotest in
  run "Skew"
    [
      ( "is_canonical",
        List.map
          (fun a -> test a is_canonical bool)
          [
            ([], true, "[]");
            (s1, true, "(T, 0) :: (O, 3) :: (O, 0) :: []");
            (s3, true, "(O, 0) :: []");
            ([ (T, 1); (T, 1) ], false, "[ (T, 1); (T, 1) ];");
            ([ (O, 1); (T, 1) ], false, "[ (O, 1); (T, 1) ]");
          ] );
      ( "is_well_formed",
        List.map
          (fun a -> test a is_well_formed bool)
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
          ] );
      ( "skew_to_int",
        List.map
          (fun a -> test a skew_to_int int)
          [
            ([], 0, "[]");
            (only_one, pow_2 (50 + 1) - 2 - 50, "Sum 1 * (2^(k+1) - 1)");
            (s1, pow_2 5 + pow_2 6, "mixed");
            (s2, pow_2 12, "mixed");
          ] );
      ( "inc",
        List.map
          (fun a -> test a (fun x -> skew_to_int (inc x)) int)
          (List.map
             (fun s -> (s, skew_to_int s + 1, Format.asprintf "%a" pp_skew s))
             [ s1; s2; s3; s4 ]) );
      ("inc (QCheck)", [ QCheck_alcotest.to_alcotest test_inc_q ]);
      ("dec (QCheck)", [ QCheck_alcotest.to_alcotest test_dec_q ]);
      ( "cons",
        List.map
          (fun a -> test_2 a (cons 1) equal_skew_tree)
          [
            ( skew_tree1,
              [
                (1, Two (0, tree1, Leaf 1));
                (7, One (1, tree3));
                (15, One (0, tree4));
              ],
              true,
              "[ (1, Two (0, tree1, Leaf 1)); (7, One (1, tree3)); (15, One \
               (0, tree4)) ]" );
            ( skew_tree2,
              [ (15, Two (3, tree4, Node (1, tree3, tree3))) ],
              true,
              "[ (15, Two (3, tree4, Node (1, tree3, tree3))) ]" );
          ] );
      ( "head",
        List.map
          (fun a -> test a head int)
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
        List.map
          (fun a -> test_2 a tail equal_skew_tree)
          [
            ( skew_tree1,
              [ (7, One (1, tree3)); (15, One (0, tree4)) ],
              true,
              "[ (1, One (0, tree1)); (7, One (1, tree3)); (15, One (0, \
               tree4)) ]" );
            ( skew_tree2,
              [
                (3, Two (1, tree2, tree2));
                (7, One (0, tree3));
                (15, One (0, tree4));
              ],
              true,
              "[ (7, Two (2, tree3, tree3)); (15, One (0, tree4)) ]" );
          ] );
      ("lookup tree", [ test_lookup_tree ]);
      ("lookup", [ test_lookup ]);
      ( "update_tree",
        List.map
          (fun a ->
            test_2 a (fun x -> update_tree 100 15 x tree_lookup_test) equal_tree)
          [
            ( 0,
              Node
                ( 100,
                  Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7)),
                  Node
                    (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14))
                ),
              true,
              "0" );
            ( 6,
              Node
                ( 0,
                  Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 100, Leaf 7)),
                  Node
                    (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14))
                ),
              true,
              "6" );
          ] );
      ( "update",
        List.map
          (fun a ->
            test_2 a (fun x -> update 100 x lookup_test) equal_skew_tree)
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
              true,
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
              true,
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
              true,
              "6" );
          ] );
    ]
