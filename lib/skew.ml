type digit = O | T
type skew = (digit * int) list
type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

type 'a array_digit =
  | One of (int * 'a tree)
  | Two of (int * 'a tree * 'a tree)

(*int same as in skew*)
type 'a skew_tree = (int * 'a array_digit) list

let rec pow_2 n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let y = pow_2 (n / 2) in
    y * y
  else 2 * pow_2 (n - 1)

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
  aux s 1 2

let rec card = function
  | Leaf _ -> 1
  | Node (_, t1, t2) -> 1 + card t1 + card t2

(*affiche dans l'ordre naturel*)
let rec pp_skew fmt (s : skew) =
  match s with
  | [] -> Format.fprintf fmt "•"
  | (w, n) :: rest ->
      pp_skew fmt rest;
      Format.fprintf fmt " %d %s"
        (if w = O then 1 else 2)
        (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))

(*affiche dans l'ordre naturel*)
let rec pp_skew_tree fmt = function
  | [] -> Format.fprintf fmt "•"
  | (_, One (n, t)) :: rest ->
      pp_skew_tree fmt rest;
      Format.fprintf fmt " (1 * card %d ) %s" (card t)
        (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))
  | (_, Two (n, t1, t2)) :: rest ->
      pp_skew_tree fmt rest;
      Format.fprintf fmt " (2 * card %d * card %d) %s" (card t1) (card t2)
        (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))

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
        print_string
          ("One (" ^ string_of_int n ^ " , card : "
          ^ string_of_int (card t)
          ^ " ) acc : " ^ string_of_int acc ^ " \n");
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

let equal s1 s2 =
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
    s1 s2

let inc : skew -> skew = function
  | [] -> (O, 0) :: []
  | (T, n) :: [] -> (O, n + 1) :: []
  | (O, n) :: [] -> if n = 0 then (T, n) :: [] else [ (O, 0); (O, n - 1) ]
  | (w1, n1) :: (w2, n2) :: rest ->
      if w1 = w2 then
        if n1 = 0 then (T, 0) :: (w2, n2) :: rest
        else (O, 0) :: (w1, n1 - 1) :: (w2, n2) :: rest
      else if n2 = 0 then (T, n1 + 1) :: rest
      else (O, n1 + 1) :: (w2, n2 - 1) :: rest
(*w1 = w2 => w1 = w2 = 1, sinon w1 = 2, w2 = 1*)

let dec : skew -> skew = function
  | [] -> failwith "underflow"
  | (T, 0) :: rest -> (O, 0) :: rest
  | (O, 0) :: (w, n) :: rest -> (w, n + 1) :: rest
  | (O, 0) :: [] -> []
  | (T, n) :: rest -> (T, n - 1) :: (O, 0) :: rest
  | (O, n) :: (w, n2) :: rest -> (T, n - 1) :: (w, n2 + 1) :: rest
  | (O, n) :: [] -> (T, n - 1) :: []

let cons x st =
  match st with
  | [] -> (1, One (0, Leaf x)) :: []
  | (w, Two (n, t1, t2)) :: [] ->
      ((2 * w) - 1, One (n + 1, Node (x, t1, t2))) :: []
  | (w, One (n, t)) :: [] ->
      if n = 0 then (w, Two (n, t, Leaf x)) :: []
      else [ (1, One (0, Leaf x)); (w, One (n - 1, t)) ]
  | (w1, One (n1, t1)) :: (w2, One (n2, t2)) :: rest ->
      (*/!\ non commutatif*)
      if n1 = 0 then (1, Two (0, t1, Leaf x)) :: (w2, One (n2, t2)) :: rest
      else
        (1, One (0, Leaf x))
        :: (w1, One (n1 - 1, t1))
        :: (w2, One (n2, t2))
        :: rest
  | (w1, Two (n1, t1, t2)) :: (w2, One (n2, t3)) :: rest ->
      if n2 = 0 then (w2, Two (n1 + 1, t3, Node (x, t1, t2))) :: []
      else
        ((w1 * 2) - 1, One (n1 + 1, Node (x, t1, t2)))
        :: (w2, One (n2, t3))
        :: rest
  | _ -> [] (*nécessaire car on suppose Myers*)

let head = function
  | [] -> failwith "empty"
  | (1, One (0, Leaf a)) :: _ -> a
  | (_, One (_, Node (a, _, _))) :: _ -> a
  | (1, Two (0, Leaf a, Leaf _)) :: _ -> a
  | (_, Two (_, Node (a, _, _), Node _)) :: _ -> a
  | _ -> failwith "incorrect form"

let tail = function
  | [] -> failwith "empty"
  | (1, One (0, Leaf _)) :: rest -> rest
  | (w, One (n, Node (_, t1, t2))) :: rest -> (w / 2, Two (n, t1, t2)) :: rest
  | (1, Two (0, Leaf _, Leaf t2)) :: rest -> (1, One (0, Leaf t2)) :: rest
  | (w, Two (n, Node (_, t1, t2), t3)) :: rest ->
      (w / 2, Two (n - 1, t1, t2)) :: (w, One (0, t3)) :: rest
  | _ -> failwith "incorrect form"

let rec lookup_tree w i t =
  if i < 0 || i > w then failwith "i out of bounds"
  else
    match (w, i, t) with
    | 1, 0, Leaf x -> x
    | _, 0, Node (x, _, _) -> x
    | w, i, Node (_, t1, t2) ->
        if i <= w / 2 then lookup_tree (w / 2) (i - 1) t1
        else lookup_tree (w / 2) (i - 1 - (w / 2)) t2
    | _ -> failwith "incorrect form"

let rec lookup i = function
  | [] -> failwith "empty"
  | (w, One (_, t)) :: ts ->
      if i < w then lookup_tree w i t else lookup (i - w) ts
  | (w, Two (_, t1, t2)) :: ts ->
      if i < w then lookup_tree w i t1
      else lookup (i - w) ((w, One (0, t2)) :: ts)
