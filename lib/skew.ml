type digit = O | T
type skew = (digit * int) list

type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
[@@deriving show, eq]

type 'a array_digit =
  | One of (int * 'a tree)
  | Two of (int * 'a tree * 'a tree)
[@@deriving show, eq]

(*int same as in skew*)
type 'a skew_tree = (int * 'a array_digit) list [@@deriving show, eq]

let rec pow_2 n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let y = pow_2 (n / 2) in
    y * y
  else 2 * pow_2 (n - 1)

let rec card = function
  | Leaf _ -> 1
  | Node (_, t1, t2) -> 1 + card t1 + card t2

let rec height = function Leaf _ -> 0 | Node (_, _, t) -> 1 + height t

let is_canonical : skew -> bool = function
  | (T, n) :: rest ->
      n >= 0 && List.for_all (fun (w, n) -> w = O && n >= 0) rest
  | l -> List.for_all (fun (w, n) -> w = O && n >= 0) l

(*check myers form and cardinal of each tree is correct*)
let is_well_formed s =
  let rec aux s acc =
    match s with
    | [] -> true
    | (w, One (n, t)) :: rest ->
        let new_acc = 2 * acc in
        if n = 0 then w = card t && card t = acc - 1 && aux rest new_acc
        else aux ((w, One (n - 1, t)) :: rest) new_acc
    | _ -> false
  in
  match s with
  | [] -> true
  | (_, One (_, _)) :: _ -> aux s 2
  | (w, Two (n, t1, t2)) :: rest ->
      w = card t1
      && card t1 = card t2
      && card t1 = pow_2 (n + 1) - 1
      &&
      let acc = pow_2 (n + 2) in
      aux rest acc

let skew_to_int s =
  let rec aux s acc indice =
    match s with
    | [] -> 0
    | (d, n) :: rest ->
        let w = if d = O then 1 else 2 in
        let new_acc = acc + indice in
        let new_indice = 2 * indice in
        if n = 0 then (w * acc) + aux rest new_acc new_indice
        else aux ((d, n - 1) :: rest) new_acc new_indice
  in
  assert (is_canonical s);
  aux s 1 2

let skew_from_int n =
  let smallest_pow k pow n =
    let rec smallest_pow k pow i =
      assert (1 lsl (k + 1) = pow);
      if i < (2 * pow) - 1 then (
        let k, r = (k, pow - 1) in
        assert (r = (1 lsl (k + 1)) - 1);
        assert (n >= r && n <= 2 * r);
        (k, r))
      else smallest_pow (k + 1) (2 * pow) i
    in
    smallest_pow k pow n
  in
  (*return skew with int : distance to the start*)
  let rec aux n =
    if n = 0 then []
    else
      let k, pow = smallest_pow 0 2 n in
      if n = 2 * pow then (T, k) :: [] else (O, k) :: aux (n - pow)
  in
  (*takes skew with int : distance to the start and makes it skew with int : distance to the previous*)
  let rec compose (a, n) l =
    match l with
    | [] -> (a, n) :: []
    | (b, m) :: [] -> [ (a, n - m - 1); (b, m) ]
    | (O, m) :: rest -> (a, n - m - 1) :: compose (O, m) rest
    | _ -> assert false
  in
  match aux n with [] -> [] | a :: rest -> List.rev (compose a rest)

let pp_card_tree fmt (n, t) =
  let rec pp_int_tree fmt t =
    match t with
    | Leaf _ -> Format.fprintf fmt "Leaf 'a"
    | Node (_, t1, t2) ->
        Format.fprintf fmt "Node ('a, %a, %a)" pp_int_tree t1 pp_int_tree t2
  in
  Format.fprintf fmt "card : %d | %a" n pp_int_tree t

(*affiche dans l'ordre naturel*)
let rec pp_skew fmt (s : skew) =
  assert (is_canonical s);
  match s with
  | [] -> Format.fprintf fmt "•"
  | (w, n) :: rest ->
      pp_skew fmt rest;
      Format.fprintf fmt " %d %s"
        (if w = O then 1 else 2)
        (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))

let rec pp_skew_natural fmt (s : skew) =
  match s with
  | [] -> Format.fprintf fmt "•"
  | (w, n) :: rest ->
      Format.fprintf fmt " %d [%d] %a"
        (if w = O then 1 else 2)
        n pp_skew_natural rest

(*affiche dans l'ordre naturel*)
(*let pp_skew_tree fmt st =
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

  let equal_skew_tree s1 s2 =
    let rec aux t1 t2 =
      match (t1, t2) with
      | Leaf x1, Leaf x2 -> x1 = x2
      | Node (x1, t1, t2), Node (x2, t3, t4) -> x1 = x2 && aux t1 t3 && aux t2 t4
      | _ -> false
    in
    List.for_all2
      (fun d1 d2 ->
        match (d1, d2) with
        | (w1, One (n1, t1)), (w2, One (n2, t2)) ->
            w1 = w2 && n1 = n2 && aux t1 t2
        | (w1, Two (n1, t1, t2)), (w2, Two (n2, t3, t4)) ->
            w1 = w2 && n1 = n2 && aux t1 t3 && aux t2 t4
        | _ -> false)
      s1 s2*)

let inc s =
  assert (is_canonical s);
  match s with
  | [] -> (O, 0) :: []
  | (T, n) :: [] -> (O, n + 1) :: []
  | (O, n) :: [] -> if n = 0 then (T, n) :: [] else [ (O, 0); (O, n - 1) ]
  | (O, 0) :: (O, n2) :: rest -> (T, 0) :: (O, n2) :: rest
  | (O, n1) :: (O, n2) :: rest -> (O, 0) :: (O, n1 - 1) :: (O, n2) :: rest
  | (T, n1) :: (O, 0) :: rest -> (T, n1 + 1) :: rest
  | (T, n1) :: (O, n2) :: rest -> (O, n1 + 1) :: (O, n2 - 1) :: rest
  | _ -> assert false

let dec s =
  assert (is_canonical s);
  match s with
  | [] -> raise (Failure "dec")
  | (T, 0) :: rest -> (O, 0) :: rest
  | (O, 0) :: (w, n) :: rest -> (w, n + 1) :: rest
  | (O, 0) :: [] -> []
  | (T, n) :: rest -> (T, n - 1) :: (O, 0) :: rest
  | (O, n) :: (w, n2) :: rest -> (T, n - 1) :: (w, n2 + 1) :: rest
  | (O, n) :: [] -> (T, n - 1) :: []

let sub_1 s =
  let plus_1 = function [] -> [] | (a, n) :: rest -> (a, n + 1) :: rest in
  assert (is_canonical s);
  match s with
  | [] -> raise (Failure "sub_1")
  | (O, 0) :: rest -> plus_1 rest
  | (O, n) :: rest -> (T, n - 1) :: plus_1 rest
  | (T, 0) :: rest -> (O, 0) :: rest
  | (T, n) :: rest -> (T, n - 1) :: (O, 0) :: rest

let rec sub s1 = function
  | [] -> s1
  | s2 -> if s1 = [] then raise (Failure "sub") else sub (sub_1 s1) (sub_1 s2)

let cons x st =
  assert (is_well_formed st);
  match st with
  | [] -> (1, One (0, Leaf x)) :: []
  | (w, Two (n, t1, t2)) :: [] ->
      ((2 * (w + 1)) - 1, One (n + 1, Node (x, t1, t2))) :: []
  | (w, One (0, t)) :: [] -> (w, Two (0, t, Leaf x)) :: []
  | (w, One (n, t)) :: [] -> [ (1, One (0, Leaf x)); (w, One (n - 1, t)) ]
  | (_, One (0, t1)) :: (w2, One (n2, t2)) :: rest ->
      (1, Two (0, t1, Leaf x)) :: (w2, One (n2, t2)) :: rest
  | (w1, One (n1, t1)) :: (w2, One (n2, t2)) :: rest ->
      (1, One (0, Leaf x))
      :: (w1, One (n1 - 1, t1))
      :: (w2, One (n2, t2))
      :: rest
  | (_, Two (n1, t1, t2)) :: (w2, One (0, t3)) :: rest ->
      (w2, Two (n1 + 1, t3, Node (x, t1, t2))) :: rest
  | (w1, Two (n1, t1, t2)) :: (w2, One (n2, t3)) :: rest ->
      ((2 * (w1 + 1)) - 1, One (n1 + 1, Node (x, t1, t2)))
      :: (w2, One (n2 - 1, t3))
      :: rest
  | _ -> assert false

let head st =
  assert (is_well_formed st);
  match st with
  | [] -> raise (Failure "head")
  | (1, One (0, Leaf a)) :: _ -> a
  | (_, One (_, Node (a, _, _))) :: _ -> a
  | (1, Two (0, Leaf _, Leaf a)) :: _ -> a
  | (_, Two (_, Node _, Node (a, _, _))) :: _ -> a
  | _ -> assert false

let tail st =
  let plus_1 = function
    | [] -> []
    | (w, One (n, t)) :: rest -> (w, One (n + 1, t)) :: rest
    | (w, Two (n, t1, t2)) :: rest -> (w, Two (n + 1, t1, t2)) :: rest
  in
  assert (is_well_formed st);
  match st with
  | [] -> raise (Failure "tail")
  | (1, One (0, Leaf _)) :: rest -> plus_1 rest
  | (w, One (n, Node (_, t1, t2))) :: rest ->
      (w / 2, Two (n - 1, t1, t2)) :: plus_1 rest
  | (1, Two (0, Leaf t1, Leaf _)) :: rest -> (1, One (0, Leaf t1)) :: rest
  | (w, Two (n, t3, Node (_, t1, t2))) :: rest ->
      (w / 2, Two (n - 1, t1, t2)) :: (w, One (0, t3)) :: rest
  | _ -> assert false

let rec lookup_tree w i t =
  if i < 0 || i > w then raise (Failure "lookup_tree")
  else
    match (w, i, t) with
    | 1, 0, Leaf x -> x
    | _, 0, Node (x, _, _) -> x
    | w, i, Node (_, t1, t2) ->
        if i <= w / 2 then lookup_tree (w / 2) (i - 1) t2
        else lookup_tree (w / 2) (i - 1 - (w / 2)) t1
    | _ -> assert false

let rec update_tree y w i t =
  if i < 0 || i > w then raise (Failure "update_tree")
  else
    match (w, i, t) with
    | 1, 0, Leaf _ -> Leaf y
    | _, 0, Node (_, t1, t2) -> Node (y, t1, t2)
    | w, i, Node (x, t1, t2) ->
        if i <= w / 2 then Node (x, update_tree y (w / 2) (i - 1) t1, t2)
        else Node (x, t1, update_tree y (w / 2) (i - 1 - (w / 2)) t2)
    | _ -> assert false

let lookup i st =
  let rec aux i st =
    match st with
    | [] -> raise (Failure "lookup")
    | (w, One (_, t)) :: ts ->
        if i < w then lookup_tree w i t else aux (i - w) ts
    | (w, Two (_, t1, t2)) :: ts ->
        if i < w then lookup_tree w i t2
        else aux (i - w) ((w, One (0, t1)) :: ts)
  in
  assert (is_well_formed st);
  aux i st

let to_bin st =
  let rec aux = function
    | [] -> []
    | (_, One (d, _)) :: rest -> (O, d) :: aux rest
    | (_, Two (d, _, _)) :: rest -> (T, d) :: aux rest
  in
  aux st

(* takes bins in the form returned by compose
   0 if EQ
   -1 if b1 < b2
   1 if b1 > b2
*)
let rec compare b1 b2 =
  match (b1, b2) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | (O, d) :: ts1, (O, d2) :: ts2 ->
      if d = d2 then compare ts1 ts2 else if d > d2 then 1 else -1
  | (T, d) :: ts1, (T, d2) :: ts2 ->
      if d = d2 then compare ts1 ts2 else if d > d2 then 1 else -1
  | (O, d) :: _, (T, d2) :: _ -> if d > d2 then 1 else -1
  | (T, d) :: _, (O, d2) :: _ -> if d >= d2 then 1 else -1

let compose_bin s =
  let rec aux acc = function
    | [] -> []
    | (w, d) :: ts -> (w, d + acc) :: aux (d + acc + 1) ts
  in
  aux 0 s

let uncompose_bin l =
  let rec aux (a, n) l =
    match l with
    | [] -> (a, n) :: []
    | (b, m) :: [] -> [ (a, n - m - 1); (b, m) ]
    | (O, m) :: rest -> (a, n - m - 1) :: aux (O, m) rest
    | _ -> assert false
  in
  match l with [] -> [] | (a, n) :: t -> List.rev (aux (a, n) t)

(* takes 2 composed skews and returns composed skew*)
let sub_composed s1 s2 =
  List.rev (compose_bin (sub (uncompose_bin s1) (uncompose_bin s2)))

(* i natural form distance to the start *)
let rec lookup_tree_bin (i : skew) (t : 'a tree) =
  let minus_one_msb = function
    | [] -> assert false
    | (O, _) :: tl -> tl
    | (T, d) :: tl -> (O, d) :: tl
  in
  match t with
  | Leaf a ->
      assert (i = []);
      a
  | Node (a, Leaf a2, Leaf a3) ->
      if i = [] then a
      else if i = [ (O, 0) ] then a3
      else if i = [ (T, 0) ] then a2
      else assert false
  | Node (a, t1, t2) ->
      if i = [] then a
      else
        let i_minus_one = sub_composed i [ (O, 0) ] in
        if compare i [ (O, height t - 1) ] > 0 then
          lookup_tree_bin (minus_one_msb i_minus_one) t1
        else lookup_tree_bin i_minus_one t2

let lookup_bin (i : skew) (st : 'a skew_tree) =
  let rec compose_ral acc = function
    | [] -> []
    | (w, One (d, t)) :: ts ->
        (w, One (d + acc, t)) :: compose_ral (d + acc + 1) ts
    | (w, Two (d, t1, t2)) :: ts ->
        (w, Two (d + acc, t1, t2)) :: compose_ral (d + acc + 1) ts
  in
  let rec aux i st =
    match (i, st) with
    | [], (_, One (_, t)) :: [] -> lookup_tree_bin i t
    | [], (_, Two (_, _, t)) :: [] -> lookup_tree_bin i t
    | (_, _) :: _, (_, One (_, t)) :: [] -> lookup_tree_bin i t
    | (_, d1) :: _, (_, Two (d2, t1, t2)) :: [] ->
        let uncomposed_i = uncompose_bin i in
        if d1 = d2 then
          lookup_tree_bin
            (List.rev
               (compose_bin (sub uncomposed_i (skew_from_int (card t1)))))
            t1
        else lookup_tree_bin i t2
    | [], (_, One (_, _)) :: (w, One (d3, t2)) :: ts2 ->
        aux [] ((w, One (d3, t2)) :: ts2)
    | (_, d1) :: _, (_, One (d2, t)) :: (w, One (d3, t2)) :: ts2 ->
        if d1 <= d2 && d1 >= d3 then
          if compare i (to_bin ((w, One (d3, t2)) :: ts2)) >= 0 then
            let ts = to_bin ((w, One (d3, t2)) :: ts2) in
            lookup_tree_bin (sub_composed i ts) t
          else aux i ((w, One (d3, t2)) :: ts2)
        else aux i ((w, One (d3, t2)) :: ts2)
    | [], (_, One (_, _)) :: (w, Two (d3, t2, t3)) :: ts2 ->
        aux [] ((w, Two (d3, t2, t3)) :: ts2)
    | (c, d1) :: _, (_, One (d2, t)) :: (w, Two (d3, t2, t3)) :: ts2 ->
        if d1 <= d2 && if c = O then d1 > d3 else d1 >= d3 then
          let ts = to_bin ((w, Two (d3, t2, t3)) :: ts2) in
          lookup_tree_bin (sub_composed i ts) t
        else aux i ((w, Two (d3, t2, t3)) :: ts2)
    | _ -> assert false
  in
  assert (is_well_formed st && is_canonical i);
  aux (List.rev (compose_bin i)) (List.rev (compose_ral 0 st))

let update y i st =
  let rec aux y i st =
    match st with
    | [] -> raise (Failure "update")
    | (w, One (n, t)) :: ts ->
        if i < w then (w, One (n, update_tree y w i t)) :: ts
        else (w, One (n, t)) :: aux y (i - w) ts
    | (w, Two (n, t1, t2)) :: ts ->
        if i < w then (w, Two (n, update_tree y w i t1, t2)) :: ts
        else if i < 2 * w then
          (w, Two (n, t1, update_tree y w (i - w) t2)) :: ts
        else (w, Two (n, t1, t2)) :: aux y (i - (2 * w)) ts
  in
  assert (is_well_formed st);
  aux y i st

let from_list list =
  let rec aux res list =
    match list with [] -> res | a :: rest -> aux (cons a res) rest
  in
  aux [] (List.rev list)

let to_list st =
  let rec aux = function [] -> [] | st -> head st :: aux (tail st) in
  assert (is_well_formed st);
  aux st
