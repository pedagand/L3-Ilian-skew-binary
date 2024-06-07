open Numrep.Skew

let () =
  let tree_lookup_test =
    Node
      ( 0,
        Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7)),
        Node (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14)) )
  in

  let lookup_test =
    [
      (3, Two (1, Node (15, Leaf 16, Leaf 17), Node (18, Leaf 19, Leaf 20)));
      (15, One (1, tree_lookup_test));
    ]
  in
  print_int (lookup 4 lookup_test)
