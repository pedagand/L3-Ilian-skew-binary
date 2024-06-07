open Numrep.Skew

let () =
  let tree_lookup =
    Node
      ( 0,
        Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7)),
        Node (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14)) )
  in
  print_int (lookup_tree 15 6 tree_lookup)
