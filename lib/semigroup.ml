(** A {{:https://wiki.haskell.org/Typeclassopedia#Semigroup} Semigroup} is a set with a binary operator that is associative.
    The natural numbers with the binary addition operator form a semigroup. *)
module type Semigroup = sig
  type t

  (** The binary operation. *)
  val ( <> ) : t -> t -> t

  (** Concatenates a non empty list-like like a fold using [( <> )]. *)
  val s_concat : t list -> t

  (** Repeats the element [n] times. *)
  val s_times : int -> t -> t
end

(** The module applied to this functor also need to be a [Semigroup]. *)
module MakeSemiGroup (S : sig
    type t

    val ( <> ) : t -> t -> t
  end) : Semigroup with type t = S.t = struct
  include S

  let s_concat (a : 'a list) : 'a =
    let rec go x =
      match x with
      | [] -> failwith "list must not be empty"
      | c :: [] -> c
      | c :: cs -> c <> go cs
    in
    go a
  ;;

  let s_times n x =
    let even n = Int.rem n 2 == 0 in
    (* This is a performance optimization technique name 'exponentiation by squaring'
       (https://en.wikipedia.org/wiki/Exponentiation_by_squaring)
       that allows for a complexity of O(log n), using fold here would be O(n). *)
    let rec g x y z =
      match x, y, z with
      | x, y, z when even y -> g (x <> x) (y / 2) z
      | x, y, z when y = 1 -> x <> z
      | x, y, z -> g (x <> x) ((y - 1) / 2) (x <> z)
    in
    let rec f x y =
      match x, y with
      | x, y when even y -> f (x <> x) (y / 2)
      | x, y when y = 1 -> x
      | x, y -> g (x <> x) ((y - 1) / 2) x
    in
    match n with
    | n when n <= 0 -> failwith "n must be greate 0"
    | n -> f x n
  ;;
end

module IntPlus : Semigroup with type t = int = MakeSemiGroup (struct
    type t = int

    let ( <> ) a b = a + b
  end)
