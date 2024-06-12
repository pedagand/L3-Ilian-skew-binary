(*open Numrep.Skew

  let () =
  let tree1 = Leaf 0 in
  let tree2 = Node (1, tree1, tree1) in
  let tree3 = Node (2, tree2, tree2) in
  let tree4 = Node (3, tree3, tree3) in

  let skew_tree1 : int skew_tree =
    [ (1, One (0, tree1)); (7, One (1, tree3)); (15, One (0, tree4)) ]
  in
  let skew_tree2 : int skew_tree =
    [ (7, Two (2, tree3, tree3)); (15, One (0, tree4)) ]
  in
  let eq1 = equal_skew_tree (=) skew_tree1 skew_tree2 in
  Printf.printf "tree1 and tree2 are equal: %b\n" eq1;*)
