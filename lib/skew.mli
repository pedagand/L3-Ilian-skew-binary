type digit = O | T
type skew = (digit * int) list

val skew_to_int : skew -> int
val pp_skew : Format.formatter -> skew -> unit
val is_canonical : skew -> bool
val inc : skew -> skew
val dec : skew -> skew
