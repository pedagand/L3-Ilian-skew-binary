open Numrep.Skew

let () =
  let print =
    List.iter (fun a -> Format.fprintf Format.std_formatter " %d " a)
  in
  let rec pp_int_tree fmt t =
    match t with
    | Leaf a -> Format.fprintf fmt "Leaf %d" a
    | Node (a, t1, t2) ->
        Format.fprintf fmt "Node (%d, %a, %a)" a pp_int_tree t1 pp_int_tree t2
  in

  let pp fmt st =
    let rec aux fmt st =
      match st with
      | [] -> Format.fprintf fmt "•"
      | (_, One (n, t)) :: rest ->
          aux fmt rest;
          Format.fprintf fmt " (1 * %a ) %s" pp_int_tree t
            (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))
      | (_, Two (n, t1, t2)) :: rest ->
          aux fmt rest;
          Format.fprintf fmt " (2 * %a * %a) %s" pp_int_tree t1 pp_int_tree t2
            (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))
    in
    (*assert (is_well_formed st);*)
    aux fmt st
  in
  let l = [ 1; 2; 3; 4; 5 ] in
  let res = cons 100 (from_list l) in
  pp Format.std_formatter (from_list l);
  pp Format.std_formatter res;
  print (to_list (List.rev res))
