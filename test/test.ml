let () =
  Alcotest.run
    "Typeclassopedia"
    [ "Functor", Test_functor.suite; "Applicative", Test_applicative.suite ]
;;
