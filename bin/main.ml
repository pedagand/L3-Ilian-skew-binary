open Numrep.Skew

let () =
  let rec p n =
    if n = 100 then ""
    else
      string_of_int n ^ " : "
      ^ Format.asprintf "%a" pp_skew (skew_from_int n)
      ^ "\n"
      ^ p (n + 1)
  in
  print_string (p 0)
