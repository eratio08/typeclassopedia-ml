open Typeclassopedia

module SemigroupLaws (S : Semigroup.Semigroup) = struct
  open S

  let test_associativity x y z = x <> y <> z = (x <> (y <> z))
end

let test_semigroup_laws_int_plus () =
  let open SemigroupLaws (Semigroup.IntPlus) in
  Alcotest.(check bool) "should pass associativity law" true (test_associativity 1 2 3)
;;

module IntPlusMaybe = Maybe.Semigroup (Semigroup.IntPlus)

let test_semigroup_laws_maybe_int_plus () =
  let open SemigroupLaws (IntPlusMaybe) in
  Alcotest.(check bool)
    "should pass associativity law, only Just"
    true
    (test_associativity (Just 1) (Just 2) (Just 3));
  Alcotest.(check bool)
    "should pass associativity law, only Just"
    true
    (test_associativity (Just 1) (Just 2) Nothing)
;;

let test_s_concat_int_plus () =
  Alcotest.(check int)
    "should concat given list of IntPlus"
    3
    (Semigroup.IntPlus.s_concat [ 1; 2 ])
;;

let test_s_concat_int_plus_maybe () =
  let open Maybe.Maybe in
  let int_plus_maybe =
    Alcotest.testable (Maybe.Maybe.pp (fun fmt x -> Format.fprintf fmt "%d" x)) ( = )
  in
  Alcotest.(check int_plus_maybe)
    "should concat given list of IntPlusMaybe"
    (Just 3)
    (IntPlusMaybe.s_concat [ Just 1; Just 2 ]);
  Alcotest.(check int_plus_maybe)
    "should concat given list of IntPlusMaybe"
    (Just 1)
    (IntPlusMaybe.s_concat [ Just 1; Nothing ])
;;

let suite : unit Alcotest.test_case list =
  [ "IntPlus", `Quick, test_semigroup_laws_int_plus
  ; "IntPlus.s_concat", `Quick, test_s_concat_int_plus
  ; "MaybeIntPlus", `Quick, test_semigroup_laws_maybe_int_plus
  ; "MaybeIntPlus.s_concat", `Quick, test_s_concat_int_plus_maybe
  ]
;;
