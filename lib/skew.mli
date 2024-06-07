type digit = O | T
type skew = (digit * int) list
(*int : number of zero between digit and the previous digit/the start*)

type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

type 'a array_digit =
  | One of (int * 'a tree)
  | Two of (int * 'a tree * 'a tree)

(*int same as in skew*)
type 'a skew_tree = (int * 'a array_digit) list

val skew_to_int : skew -> int
val pp_skew : Format.formatter -> skew -> unit
val pp_skew_tree : Format.formatter -> 'a skew_tree -> unit
val is_canonical : skew -> bool
val is_well_formed : 'a skew_tree -> bool

(*val equal : 'a tree -> 'a tree -> bool*)
val equal : 'a skew_tree -> 'a skew_tree -> bool
val inc : skew -> skew
val dec : skew -> skew
val cons : 'a -> 'a skew_tree -> 'a skew_tree
val card : 'a tree -> int
val head : 'a skew_tree -> 'a
val tail : 'a skew_tree -> 'a skew_tree
val lookup : int -> 'a skew_tree -> 'a
val lookup_tree : int -> int -> 'a tree -> 'a (*weight, indice, tree*)
