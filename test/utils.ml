open Numrep.Skew
open QCheck

let int_skew_tree =
  Alcotest.testable
    (pp_skew_tree (fun oc -> Format.fprintf oc "%d"))
    (equal_skew_tree ( = ))

let int_tree =
  Alcotest.testable
    (pp_tree (fun oc -> Format.fprintf oc "%d"))
    (equal_tree ( = ))

let skew =
  Alcotest.testable pp_skew (fun s1 s2 ->
      let s1 = skew_from_int (skew_to_int s1) in
      let s2 = skew_from_int (skew_to_int s2) in
      List.equal ( = ) s1 s2)

let rec pow_2 n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let y = pow_2 (n / 2) in
    y * y
  else 2 * pow_2 (n - 1)

let only_one = List.init 50 (fun _ -> (O, 0))
let s1 = [ (T, 0); (O, 3); (O, 0) ]
let s2 = [ (O, 0); (O, 10) ]
let s3 = (O, 0) :: []
let s4 = (T, 0) :: []
let tree1 = Leaf 0
let tree2 = Node (1, tree1, tree1)
let tree3 = Node (2, tree2, tree2)
let tree4 = Node (3, tree3, tree3)

let skew_tree1 : int skew_tree =
  [ (1, One (0, tree1)); (7, One (1, tree3)); (15, One (0, tree4)) ]

let skew_tree2 : int skew_tree =
  [ (7, Two (2, tree3, tree3)); (15, One (0, tree4)) ]

let not_myers : int skew_tree =
  [ (1, One (0, tree1)); (7, One (1, tree3)); (15, Two (0, tree4, tree4)) ]

let incorrect_cardinal : int skew_tree =
  [ (1, One (1, tree1)); (7, One (1, tree3)); (15, Two (0, tree4, tree4)) ]

let tree_lookup_test =
  Node
    ( 0,
      Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7)),
      Node (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14)) )

let lookup_test =
  [
    (3, Two (1, Node (15, Leaf 16, Leaf 17), Node (18, Leaf 19, Leaf 20)));
    (15, One (1, tree_lookup_test));
  ]

let generator_small_int =
  let open QCheck in
  Gen.small_int

let generator_skew : skew Gen.t =
 fun st ->
  let n = generator_small_int st in
  skew_from_int n

let rec generator_tree gen_a p st =
  if p = 1 then Leaf (Gen.generate1 ~rand:st gen_a)
  else
    Node
      ( Gen.generate1 ~rand:st gen_a,
        generator_tree gen_a (p / 2) st,
        generator_tree gen_a (p / 2) st )

let upgrade s gen_a st =
  let rec aux s gen_a st acc =
    match s with
    | [] -> []
    | (O, n) :: rest ->
        let p = pow_2 (n + acc + 1) - 1 in
        (p, One (n, generator_tree gen_a p st))
        :: aux rest gen_a st (acc + n + 1)
    | (T, n) :: rest ->
        let p = pow_2 (n + acc + 1) - 1 in
        (p, Two (n, generator_tree gen_a p st, generator_tree gen_a p st))
        :: aux rest gen_a st (acc + n + 1)
  in
  aux s gen_a st 0

let generator_skew_tree gen_a : 'a skew_tree Gen.t =
 fun st ->
  let s = generator_skew st in
  upgrade s gen_a st

let arbitrary_skew_tree =
  QCheck.make
    ~print:
      (Format.asprintf "%a" (pp_skew_tree (fun oc -> Format.fprintf oc "%d")))
    (generator_skew_tree Gen.int)

let arbitrary_skew =
  QCheck.make ~print:(Format.asprintf "%a" pp_skew) generator_skew
