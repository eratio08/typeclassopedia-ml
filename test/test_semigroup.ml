open Typeclassopedia

module SemigroupLaws (S : Semigroup.Semigroup) = struct
  open S

  let test_associativity x y z = x <> y <> z = (x <> (y <> z))
end

let test_semigroup_laws_int_plus () =
  let open SemigroupLaws (Semigroup.IntPlus) in
  Alcotest.(check bool) "should pass associativity law" true (test_associativity 1 2 3)
;;

let test_s_concat_int_plus () =
  Alcotest.(check int)
    "should concat given list of IntPlus"
    3
    (Semigroup.IntPlus.s_concat [ 1; 2 ])
;;

module IntPlusMaybe = Maybe.Semigroup (Semigroup.IntPlus)

let test_semigroup_laws_maybe_int_plus () =
  let open SemigroupLaws (IntPlusMaybe) in
  Alcotest.(check bool)
    "should pass associativity law, only Just"
    true
    (test_associativity (Just 1) (Just 2) (Just 3));
  Alcotest.(check bool)
    "should pass associativity law, single Nothing"
    true
    (test_associativity (Just 1) (Just 2) Nothing);
  Alcotest.(check bool)
    "should pass associativity law, multiple Nothing"
    true
    (test_associativity Nothing (Just 2) Nothing)
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
    Nothing
    (IntPlusMaybe.s_concat [ Just 1; Nothing ])
;;

module IntPlusEither = Either.Semigroup (struct
    module E = String
    include Semigroup.IntPlus
  end)

let test_semigroup_laws_either_int_plus () =
  let open SemigroupLaws (IntPlusEither) in
  Alcotest.(check bool)
    "should pass associativity law"
    true
    (test_associativity (Right 1) (Right 2) (Right 3))
;;

let test_s_concat_int_plus_either () =
  let open Either.Either (String) in
  let int_plus_either =
    Alcotest.testable
      (pp
         (fun fmt e -> Format.fprintf fmt "%s" e)
         (fun fmt a -> Format.fprintf fmt "%d" a))
      ( = )
  in
  Alcotest.(check int_plus_either)
    "should concat given list of IntPlusEither"
    (Right 3)
    (IntPlusEither.s_concat [ Right 1; Right 2 ])
;;

let suite : unit Alcotest.test_case list =
  [ "IntPlus", `Quick, test_semigroup_laws_int_plus
  ; "IntPlus.s_concat", `Quick, test_s_concat_int_plus
  ; "MaybeIntPlus", `Quick, test_semigroup_laws_maybe_int_plus
  ; "MaybeIntPlus.s_concat", `Quick, test_s_concat_int_plus_maybe
  ; "EitherIntPlus", `Quick, test_semigroup_laws_either_int_plus
  ; "EitherIntPlus.s_concat", `Quick, test_s_concat_int_plus_either
  ]
;;
