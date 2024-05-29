type weight = W of int
type skew = (weight * int) list

let skew_to_int s =
  let rec aux s acc indice =
    match s with
    | [] -> 0
    | (W w, n) :: rest ->
        let new_acc = acc + indice in
        let new_indice = 2 * indice in
        if n = 0 then (w * acc) + aux rest new_acc new_indice
        else aux ((W w, n - 1) :: rest) new_acc new_indice
  in
  aux s 1 2

let rec pp_skew (s : skew) =
  match s with
  | [] -> "0"
  | (W w, n) :: rest ->
      pp_skew rest ^ " " ^ string_of_int w ^ ".." ^ string_of_int n ^ " "

let inc : skew -> skew = function
  | [] -> (W 1, 0) :: []
  | (W w, n) :: [] ->
      if w = 2 then (W 1, n + 1) :: []
      else if n = 0 then (W 2, n) :: []
      else [ (W 1, 0); (W w, n - 1) ]
  | (W w1, n1) :: (W w2, n2) :: rest ->
      if w1 = w2 then
        if n1 = 0 then (W 2, 0) :: (W w2, n2) :: rest
        else (W 1, 0) :: (W w1, n1 - 1) :: (W w2, n2) :: rest
      else if n2 = 0 then (W 2, n1 + 1) :: rest
      else (W 1, n1 + 1) :: (W w2, n2 - 1) :: rest
(*w1 = w2 => w1 = w2 = 1, sinon w1 = 2, w2 = 1*)
