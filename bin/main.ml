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

  let lookup_test =
    [
      (3, One (1, Node (6, Leaf 8, Leaf 5)));
      (7, One (0, Node (5, Node (38, Leaf 7, Leaf 3), Node (5, Leaf 3, Leaf 7))));
      ( 31,
        One
          ( 1,
            Node
              ( 2,
                Node
                  ( 9,
                    Node (4, Node (3, Leaf 1, Leaf 2), Node (2, Leaf 7, Leaf 3)),
                    Node (87, Node (7, Leaf 8, Leaf 8), Node (1, Leaf 0, Leaf 5))
                  ),
                Node
                  ( 44,
                    Node (6, Node (83, Leaf 5, Leaf 4), Node (6, Leaf 1, Leaf 1)),
                    Node (9, Node (8, Leaf 0, Leaf 7), Node (34, Leaf 6, Leaf 2))
                  ) ) ) );
    ]
  in

  let rec k i =
    if i = 3 + 7 + 31 then ()
    else (
      Format.fprintf Format.std_formatter
        "i : %d | lookup : %d  |  lookup_bin : %d\n" i (lookup i lookup_test)
        (lookup_bin (skew_from_int i) lookup_test);
      k (i + 1))
  in

  Format.fprintf Format.std_formatter "tree : %a\n" my_pp_skew_tree lookup_test;
  k 0
