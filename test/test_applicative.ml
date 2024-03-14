open Typeclassopedia.Applicative
open Typeclassopedia.Symb

module ApplicativeLaws (A : Applicative) = struct
  open A

  let id x = x
  let test_identity x = pure id <*> x = x
  let test_homomorphism f x = pure f <*> pure x = pure (f x)
  let test_interchange f x = f <*> pure x = (pure (fun g -> g x) <*> f)
  let test_composition u v w = u <*> (v <*> w) = (pure ( << ) <*> u <*> v <*> w)
end

let test_applicative_laws_maybe () =
  let open ApplicativeLaws (Maybe) in
  Alcotest.(check bool) "should pass identity law with Just" true (test_identity (Just 1));
  Alcotest.(check bool)
    "should pass identity law with Nothing"
    true
    (test_identity Nothing);
  Alcotest.(check bool)
    "should pass homomorphism law"
    true
    (test_homomorphism (fun x -> x * 15) 1);
  Alcotest.(check bool)
    "should pass interchange law with Just"
    true
    (test_interchange (Just (fun x -> x * 15)) 1);
  Alcotest.(check bool)
    "should pass interchange law with Nothing"
    true
    (test_interchange Nothing 1);
  Alcotest.(check bool)
    "should pass composition law all Just"
    true
    (test_composition (Just (fun x -> x * 15)) (Just (fun x -> x + 2)) (Just 1));
  Alcotest.(check bool)
    "should pass composition law w is Nothing"
    true
    (test_composition (Just (fun x -> x * 15)) (Just (fun x -> x - 2)) Nothing);
  Alcotest.(check bool)
    "should pass composition law v is Nothing"
    true
    (test_composition (Just (fun x -> x * 15)) Nothing (Just 1));
  Alcotest.(check bool)
    "should pass composition law u is Nothing"
    true
    (test_composition Nothing (Just (fun x -> x - 2)) (Just 1))
;;

let test_applicative_laws_either () =
  let open ApplicativeLaws (Either (String)) in
  Alcotest.(check bool)
    "should pass identity law with Just"
    true
    (test_identity (Right 1));
  Alcotest.(check bool)
    "should pass identity law with Nothing"
    true
    (test_identity (Left ""));
  Alcotest.(check bool)
    "should pass homomorphism law"
    true
    (test_homomorphism (fun x -> x * 15) 1);
  Alcotest.(check bool)
    "should pass interchange law with Just"
    true
    (test_interchange (Right (fun x -> x * 15)) 1);
  Alcotest.(check bool)
    "should pass interchange law with Nothing"
    true
    (test_interchange (Left "") 1);
  Alcotest.(check bool)
    "should pass composition law all Just"
    true
    (test_composition (Right (fun x -> x * 15)) (Right (fun x -> x + 2)) (Right 1));
  Alcotest.(check bool)
    "should pass composition law w is Nothing"
    true
    (test_composition (Right (fun x -> x * 15)) (Right (fun x -> x - 2)) (Left ""));
  Alcotest.(check bool)
    "should pass composition law v is Nothing"
    true
    (test_composition (Right (fun x -> x * 15)) (Left "") (Right 1));
  Alcotest.(check bool)
    "should pass composition law u is Nothing"
    true
    (test_composition (Left "") (Right (fun x -> x - 2)) (Right 1))
;;

let suite : unit Alcotest.test_case list =
  [ "Maybe", `Quick, test_applicative_laws_maybe
  ; "Either", `Quick, test_applicative_laws_either
  ]
;;
