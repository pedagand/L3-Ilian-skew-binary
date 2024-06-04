type digit = O | T
type skew = (digit * int) list
type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

type 'a array_digit =
  | One of (int * 'a tree)
  | Two of (int * 'a tree * 'a tree)

(*int same as in skew*)
type 'a skew_tree = 'a array_digit list

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
  | One (n, t) :: rest ->
      pp_skew_tree fmt rest;
      Format.fprintf fmt " (1 * card %d ) %s" (card t)
        (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))
  | Two (n, t1, t2) :: rest ->
      pp_skew_tree fmt rest;
      Format.fprintf fmt " (2 * card %d * card %d) %s" (card t1) (card t2)
        (String.init (2 * n) (fun n -> if n mod 2 = 0 then '0' else ' '))

let is_canonical : skew -> bool = function
  | (T, n) :: rest ->
      n >= 0 && List.for_all (fun (w, n) -> w = O && n >= 0) rest
  | l -> List.for_all (fun (w, n) -> w = O && n >= 0) l

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
  | [] -> One (0, Leaf x) :: []
  | Two (n, t1, t2) :: [] -> One (n + 1, Node (x, t1, t2)) :: []
  | One (n, t) :: [] ->
      if n = 0 then Two (n, t, Leaf x) :: []
      else [ One (0, Leaf x); One (n - 1, t) ]
  | One (n1, t1) :: One (n2, t2) :: rest ->
      if n1 = 0 then Two (0, t1, Leaf x) :: One (n2, t2) :: rest
      else One (0, Leaf x) :: One (n1 - 1, t1) :: One (n2, t2) :: rest
  | Two (n1, t1, t2) :: One (n2, t3) :: rest ->
      if n2 = 0 then Two (n1 + 1, Node (x, t1, t2), t3) :: []
      else One (n1 + 1, Node (x, t1, t2)) :: One (n2, t3) :: rest
  | _ -> [] (*nécessaire car on suppose Myers*)
