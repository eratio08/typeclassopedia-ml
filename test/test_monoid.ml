open Typeclassopedia

module MonoidLaws (M : Monoid.Monoid) = struct
  open M

  let test_left_identity x = m_append m_empty x = x
  let test_right_identity x = m_append x m_empty = x
  let test_associativity x y z = m_append (m_append x y) z = m_append x (m_append z y)
end

let test_monoid_laws_int_plus () =
  let open MonoidLaws (Monoid.IntPlus) in
  Alcotest.(check bool) "should pass left identity law" true (test_left_identity 1);
  Alcotest.(check bool) "should pass right identity law" true (test_right_identity 1);
  Alcotest.(check bool) "should pass asociativity law" true (test_associativity 1 2 3)
;;

let suite : unit Alcotest.test_case list =
  [ "IntPlus", `Quick, test_monoid_laws_int_plus ]
;;
