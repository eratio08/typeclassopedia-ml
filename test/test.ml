let () =
  Alcotest.run
    "Typeclassopedia"
    [ "Functor", Test_functor.suite
    ; "Applicative", Test_applicative.suite
    ; "Monad", Test_monad.suite
    ; "Semigroup", Test_semigroup.suite
    ]
;;
