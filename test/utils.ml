open Numrep.Skew

let only_one = (List.init 50 (fun _ -> (W 1, 0)))

let s1 = ((W 2, 0) :: (W 1, 3) :: (W 1, 0) :: [])

let s2 = ((W 1, 0) :: (W 1, 10) :: [])
let s3 = (((W 1, 0) :: []))
let s4 = (((W 2, 0) :: []))