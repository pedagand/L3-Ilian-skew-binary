open Numrep.Skew

let () =
  let tree1 = Leaf 0 in
  let tree2 = Node (0, tree1, tree1) in
  let tree3 = Node (0, tree2, tree2) in
  let tree4 = Node (0, tree3, tree3) in

  let skew_tree1 : int skew_tree = [ Two (2, tree3, tree3); One (0, tree4) ] in

  pp_skew_tree Format.std_formatter skew_tree1;
  let p = if is_well_formed skew_tree1 then "true" else "false" in
  print_newline ();
  print_string p;
  print_newline ()
