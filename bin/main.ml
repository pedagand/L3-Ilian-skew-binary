open Numrep.Skew

let () =
  let my_pp_skew_tree fmt st =
    let rec aux fmt st =
      match st with
      | [] -> Format.fprintf fmt "•"
      | (_, One (n, t)) :: rest ->
          aux fmt rest;
          Format.fprintf fmt " (1 * card %d ) %s" (card t)
            (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))
      | (_, Two (n, t1, t2)) :: rest ->
          aux fmt rest;
          Format.fprintf fmt " (2 * card %d * card %d) %s" (card t1) (card t2)
            (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))
    in
    assert (is_well_formed st);
    aux fmt st
  in
  let tree_lookup_test =
    Node
      ( 0,
        Node (1, Node (2, Leaf 3, Leaf 4), Node (5, Leaf 6, Leaf 7)),
        Node (8, Node (9, Leaf 10, Leaf 11), Node (12, Leaf 13, Leaf 14)) )
  in

  (*let lookup_test =
      [
        (3, Two (1, Node (15, Leaf 16, Leaf 17), Node (18, Leaf 19, Leaf 20)));
        (15, One (1, tree_lookup_test));
      ]
    in



    let rec k i =
      if i = 20 then ()
      else
        Format.fprintf Format.std_formatter
          "i : %d | lookup : %d  |  lookup_bin : %d\n" i (lookup i lookup_test)
          (lookup_bin (skew_from_int i) lookup_test);
      k (i + 1)
    in

    Format.fprintf Format.std_formatter "tree : %a\n" my_pp_skew_tree lookup_test;
    k 0*)
  let test_one =
    [ (3, One (1, Node (1, Leaf 2, Leaf 3))); (15, One (1, tree_lookup_test)) ]
  in
  Format.fprintf Format.std_formatter "tree : %a\n" my_pp_skew_tree test_one;
  let i = 3 in
  Format.fprintf Format.std_formatter
    "i : %d | lookup : %d  |  lookup_bin : %d\n" i (lookup i test_one)
    (lookup_bin (skew_from_int i) test_one)
