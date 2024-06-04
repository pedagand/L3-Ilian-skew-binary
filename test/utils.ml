open Numrep.Skew

let only_one = List.init 50 (fun _ -> (O, 0))
let s1 = [ (T, 0); (O, 3); (O, 0) ]
let s2 = [ (O, 0); (O, 10) ]
let s3 = (O, 0) :: []
let s4 = (T, 0) :: []
let tree1 = Leaf 0
let tree2 = Node (0, tree1, tree1)
let tree3 = Node (0, tree2, tree2)
let tree4 = Node (0, tree3, tree3)

let skew_tree1 : int skew_tree =
  [ One (0, tree1); One (0, tree3); One (0, tree4) ]

let skew_tree2 : int skew_tree = [ One (2, tree3); Two (0, tree4, tree4) ]
