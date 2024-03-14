open Typeclassopedia.Functor

module FunctorLaws (F : Functor) = struct
  open F

  let test_identity ?(eq = ( = )) x = eq (fmap (fun i -> i) x) x

  let test_composition ?(eq = ( = )) g h x =
    eq (fmap (fun x -> g @@ h @@ x) x) (fmap g @@ fmap h @@ x)
  ;;
end

let test_functor_laws_maybe () =
  let open FunctorLaws (Maybe) in
  Alcotest.(check bool) "should pass identity law with Just" true (test_identity (Just 1));
  Alcotest.(check bool)
    "should pass indentity law with Nothing"
    true
    (test_identity Nothing);
  Alcotest.(check bool)
    "should pass composition law with Just"
    true
    (test_composition (fun x -> x + 1) (fun x -> x * 2) (Just 1));
  Alcotest.(check bool)
    "should pass composition law with Nothing"
    true
    (test_composition (fun x -> x + 1) (fun x -> x * 2) Nothing)
;;

let test_functor_laws_either () =
  let open FunctorLaws (Either (String)) in
  Alcotest.(check bool)
    "should pass identity law with Left"
    true
    (test_identity (Left "a"));
  Alcotest.(check bool)
    "should pass indentity law with Right"
    true
    (test_identity (Right 1));
  Alcotest.(check bool)
    "should pass composition law with Left"
    true
    (test_composition (fun x -> x + 1) (fun x -> x * 2) (Left "a"));
  Alcotest.(check bool)
    "should pass composition law with Right"
    true
    (test_composition (fun x -> x + 1) (fun x -> x * 2) (Right 1))
;;

let test_functor_laws_pair () =
  let open FunctorLaws (Pair) in
  Alcotest.(check bool) "should pass identity law" true (test_identity (Pair (1, 2)));
  Alcotest.(check bool)
    "should pass composition law"
    true
    (test_composition (fun x -> x + 1) (fun x -> x * 2) (Pair (2, 3)))
;;

let test_functor_laws_tuple_section () =
  let open FunctorLaws (TupleSection (String)) in
  Alcotest.(check bool) "should pass identity law" true (test_identity ("1", 2));
  Alcotest.(check bool)
    "should pass composition law"
    true
    (test_composition (fun x -> x + 1) (fun x -> x * 2) ("2", 3))
;;

let test_functor_laws_itree () =
  let open FunctorLaws (ITree) in
  let rec eq t1 t2 =
    let open Typeclassopedia.Type.ITree in
    match t1, t2 with
    (* A bit hacky, to not generate id's for functions.*)
    | Leaf f, Leaf g -> f 10 = g 10
    | Node ns, Node ys -> List.equal eq ns ys
    | _, _ -> false
  in
  Alcotest.(check bool)
    "should pass identity law with Leaf"
    true
    (test_identity ~eq (Leaf (fun x -> x + 1)));
  Alcotest.(check bool)
    "should pass identity law with Node"
    true
    (test_identity ~eq (Node [ Leaf (fun x -> x + 1) ]));
  Alcotest.(check bool)
    "should pass composition law with Leaf"
    true
    (test_composition ~eq (fun x -> x + 1) (fun x -> x * 2) (Leaf (fun x -> x + 1)));
  Alcotest.(check bool)
    "should pass composition law with Node"
    true
    (test_composition
       ~eq
       (fun x -> x + 1)
       (fun x -> x * 2)
       (Node [ Leaf (fun x -> x + 1) ]))
;;

let suite : unit Alcotest.test_case list =
  [ "Maybe", `Quick, test_functor_laws_maybe
  ; "Either", `Quick, test_functor_laws_either
  ; "Pair", `Quick, test_functor_laws_pair
  ; "TupleSection", `Quick, test_functor_laws_tuple_section
  ; "ITree", `Quick, test_functor_laws_itree
  ]
;;
