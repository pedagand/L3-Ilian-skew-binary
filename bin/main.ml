open Numrep.Skew

let tree1 = Leaf 0
let tree2 = Node (1, tree1, tree1)
let tree3 = Node (2, tree2, tree2)
let tree4 = Node (3, tree3, tree3)

let () =
  pp_skew_tree
    (fun oc -> Format.fprintf oc "%d")
    Format.std_formatter
    [ (1, One (0, tree1)); (7, One (1, tree3)); (15, One (0, tree4)) ]
