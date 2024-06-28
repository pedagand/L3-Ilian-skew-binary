open Numrep.Skew

let () =
  (*let rec pp_sub n =
      if n = 20 then ()
      else (
        Format.fprintf Format.std_formatter
          "n : %d  |  skew-bin : %a  |  skew-bin-1 : %a | sub_1 : %a \n" n pp_skew
          (skew_from_int n) pp_skew
          (skew_from_int (n - 1))
          pp_skew
          (sub_1 (skew_from_int n));
        pp_sub (n + 1))
    in
    pp_sub 1*)
  let rec aff n acc =
    if n > 20 then ()
    else if n < acc then aff (n + 1) 1
    else
      let x = skew_from_int n in
      let y = skew_from_int acc in
      Format.fprintf Format.std_formatter "x : %a  |  y : %a  |  x - y : %a\n"
        pp_skew x pp_skew y pp_skew (sub x y);
      aff n ((2 * acc) + 1)
  in

  aff 2 1
(*let x,y = 15,3 in
  let x = skew_from_int x in
  let y = skew_from_int y in
  Format.fprintf (Format.std_formatter) "x : %a  |  y : %a  |  x - y : %a" pp_skew x pp_skew y pp_skew (sub x y)*)
