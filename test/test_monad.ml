open Typeclassopedia

module MonadLaws (M : Monad.Monad) = struct
  open M

  let test_left_identity f x = return x >>= f = f x
  let test_right_identity t = t >>= return = t
  let test_associativity t f g = t >>= f >>= g = (t >>= fun x -> f x >>= g)
end

let test_monad_laws_maybe () =
  let open MonadLaws (Maybe.Monad) in
  Alcotest.(check bool)
    "should pass left identiy law with Just"
    true
    (test_left_identity (fun x -> Just (x * 2)) 2);
  Alcotest.(check bool)
    "should pass left identiy law with Nothing"
    true
    (test_left_identity (fun _ -> Nothing) 2);
  Alcotest.(check bool)
    "should pass right identity law with Nothing"
    true
    (test_right_identity Nothing);
  Alcotest.(check bool)
    "should pass right identity law with Just"
    true
    (test_right_identity (Just 2));
  Alcotest.(check bool)
    "should pass associativity law with Just"
    true
    (test_associativity (Just 2) (fun x -> Just (2 * x)) (fun y -> Just (y + 2)));
  Alcotest.(check bool)
    "should pass associativity law with Nothing"
    true
    (test_associativity Nothing (fun x -> Just (2 * x)) (fun y -> Just (y + 2)))
;;

let test_monad_laws_either () =
  let open MonadLaws (Either.Monad (String)) in
  Alcotest.(check bool)
    "should pass left identity law with Right"
    true
    (test_left_identity (fun x -> Right x) 2);
  Alcotest.(check bool)
    "should pass left identity law with Left"
    true
    (test_left_identity (fun _ -> Left "") 2);
  Alcotest.(check bool)
    "should pass left identity law with Right"
    true
    (test_right_identity (Right 2));
  Alcotest.(check bool)
    "should pass left identity law with Left"
    true
    (test_right_identity (Left ""));
  Alcotest.(check bool)
    "should pass associativity law with Right"
    true
    (test_associativity (Right 2) (fun x -> Right (2 * x)) (fun y -> Right (y + 2)));
  Alcotest.(check bool)
    "should pass associativity law with Left"
    true
    (test_associativity (Left "") (fun x -> Right (2 * x)) (fun y -> Right (y + 2)))
;;

let suite : unit Alcotest.test_case list =
  [ "Maybe", `Quick, test_monad_laws_maybe; "Either", `Quick, test_monad_laws_either ]
;;
