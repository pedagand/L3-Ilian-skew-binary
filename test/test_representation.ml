open Numrep.Skew
open Utils

let test_skew_to_int_1 =
  let result = skew_to_int [] in
  let desired = 0 in
  Alcotest.test_case "[]" `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let rec pow_2 n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let y = pow_2 (n / 2) in
    y * y
  else 2 * pow_2 (n - 1)

(* Sum 2^(k+1) - 1 for k = 1 : n equals 2^(n + 1) - 1 - n*)
let test_skew_to_int_2 =
  let result = skew_to_int only_one in
  let desired = pow_2 (50 + 1) - 2 - 50 in
  Alcotest.test_case "Sum 1 * (2^(k+1) - 1)" `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let test_skew_to_int_3 =
  let result = skew_to_int s1 in
  let desired = pow_2 5 + pow_2 6 in
  Alcotest.test_case "mixed" `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let test_skew_to_int_4 =
  let result = skew_to_int s2 in
  let desired = pow_2 12 in
  Alcotest.test_case "mixed" `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)

let test_inc s =
  let result = skew_to_int (inc s) in
  let desired = skew_to_int s + 1 in
  Alcotest.test_case (pp_skew s) `Quick (fun () ->
      Alcotest.(check int) "same result" desired result)



let () =
  let open Alcotest in
  run "Skew"
    [
      ( "skew_to_int",
        [
          test_skew_to_int_1;
          test_skew_to_int_2;
          test_skew_to_int_3;
          test_skew_to_int_4;
        ] );
      ("incr", [ test_inc s1; test_inc s2; test_inc s3; test_inc s4 ]);
    ]
