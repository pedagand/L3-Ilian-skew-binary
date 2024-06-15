open Numrep.Skew

let () =
  let rec generator_tree x p =
    if p = 1 then Leaf x
    else Node (x, generator_tree x (p / 2), generator_tree x (p / 2))
  in
  let n = 15 in
  let t = generator_tree 1 n in
  pp_card_tree Format.std_formatter (card t, t)
