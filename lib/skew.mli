type weight = W of int
type skew = (weight * int) list

val skew_to_int : skew -> int
val pp_skew : skew -> string
val inc : skew -> skew
val dec : skew -> skew
