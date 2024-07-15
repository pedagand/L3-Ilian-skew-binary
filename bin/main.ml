open Numrep.Skew

let () =
  let tree_lookup_test =
    Node
      ( 0,
        Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7)),
        Node (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14)) )
  in

  let rec k i =
    let rec compose_bin acc = function
      | [] -> []
      | (w, d) :: ts -> (w, d + acc) :: compose_bin (d + acc + 1) ts
    in
    if i = 15 then ()
    else (
      Format.fprintf Format.std_formatter
        "i : %d | skew_i : %a | lookup_tree : %d  |  lookup_tree_bin : %d\n" i
        pp_skew_natural
        (List.rev (compose_bin 0 (skew_from_int i)))
        (lookup_tree 15 i tree_lookup_test)
        (lookup_tree_bin
           (List.rev (compose_bin 0 (skew_from_int i)))
           tree_lookup_test);
      k (i + 1))
  in

  pp_tree
    (fun oc -> Format.fprintf oc "%d")
    Format.std_formatter tree_lookup_test;
  k 0
