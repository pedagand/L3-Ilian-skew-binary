open Numrep.Skew

let () = print_string (pp_skew ((W 2, 1) :: (W 1, 0) :: (W 1, 0) :: []));
