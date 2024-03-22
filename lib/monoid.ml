module type Monoid = sig
  type t

  include Semigroup.Semigroup with type t := t

  val m_empty : t
  val m_append : t -> t -> t
  val m_concat : t list -> t
end

module MakeMonoid (S : sig
    include Semigroup.Semigroup

    val m_empty : t
  end) : Monoid with type t = S.t = struct
  include S

  let m_append a b = a <> b
  let m_concat ts = List.fold_right m_append ts m_empty
end

module IntPlus = MakeMonoid (struct
    include Semigroup.IntPlus

    let m_empty = 0
  end)
