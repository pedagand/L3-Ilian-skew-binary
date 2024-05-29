type weight = W of int
type skew = (weight * int) list

let skew_to_int s =
  let rec aux s acc indice = match s with
    |[] -> 0
    |(W w, n) :: rest -> let new_acc = acc + indice in let new_indice = 2 * indice in 
    if n = 0 then w * acc + (aux rest new_acc new_indice) else (aux ((W w, n - 1) :: rest) new_acc new_indice)
  in aux s 1 2

let rec pp_skew s = match s with
|[] -> Format.printf "0"
|(W w, n) :: rest -> pp_skew rest; Format.printf " %d..%d " w n

let inc : skew -> skew = function
|[] -> (W 1, 0) :: []
|(W w, n) :: [] -> if w = 2 then (W 1, n + 1)::[] else (W 2, n)::[]
|(W w1, n1) :: (W w2, n2) :: rest -> if w1 = w2 then [] else (W w1, n1) :: (W w2, n2) :: rest
(*w1 = w2 implies that w1 = w2 = 1*)