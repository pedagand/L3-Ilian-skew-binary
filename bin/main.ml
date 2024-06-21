open Numrep.Skew

(*let pp = List.iter (Format.fprintf Format.std_formatter " %d ")*)

let () =
  let l = [ 0; -1; 0; 0; 0 ] in
  let s = from_list l in
  print_int (lookup 0 s);
  print_newline ();
  print_int (List.nth l 0)
(*Format.fprintf Format.std_formatter "\n";
  pp_skew_tree (fun oc -> Format.fprintf oc "%d") Format.std_formatter s;
  (*
    Format.fprintf Format.std_formatter "\n head : %d \n" (head s) ;
    pp_skew_tree
    (fun oc -> Format.fprintf oc "%d")
    Format.std_formatter
    (tail s);*)
  Format.fprintf Format.std_formatter "\nto_list :\n";
  pp (to_list s)*)
