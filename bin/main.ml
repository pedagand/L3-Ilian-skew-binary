open Numrep.Skew

let () =
  let tree1 = Leaf 0 in
  let tree2 = Node (0, tree1, tree1) in
  let tree3 = Node (0, tree2, tree2) in
  let tree4 = Node (0, tree3, tree3) in

  let skew_tree1 : int skew_tree = [ One (2, tree3); Two (0, tree4, tree4) ] in

  pp_skew_tree Format.std_formatter skew_tree1
