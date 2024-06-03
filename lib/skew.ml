type digit = O | T
type skew = (digit * int) list

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

let rec pp_skew fmt (s : skew) =
  match s with
  | [] -> Format.fprintf fmt "•"
  | (w, n) :: rest ->
      pp_skew fmt rest;
      Format.fprintf fmt " %d..%d " (if w = O then 1 else 2) n

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
  | [] -> []
  | (T, 0) :: rest -> (O, 0) :: rest
  | (O, 0) :: (w, n) :: rest -> (w, n + 1) :: rest
  | (O, 0) :: [] -> []
  | (T, n) :: rest -> (T, n - 1) :: (O, 0) :: rest
  | (O, n) :: (w, n2) :: rest -> (T, n - 1) :: (w, n2 + 1) :: rest
  | (O, n) :: [] -> (T, n - 1) :: []
