open Numrep.Skew

let rec pow_2 n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let y = pow_2 (n / 2) in
    y * y
  else 2 * pow_2 (n - 1)

let rec equal_tree t1 t2 =
  match (t1, t2) with
  | Leaf a, Leaf b -> a = b
  | Node (x1, t1, t2), Node (x2, t3, t4) ->
      x1 = x2 && equal_tree t1 t3 && equal_tree t2 t4
  | _ -> false

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
