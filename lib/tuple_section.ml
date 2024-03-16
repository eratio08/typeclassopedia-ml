module TupleSection (E : sig
    type t
  end) =
struct
  type 'a t = E.t * 'a
end

module Functor (E : sig
    type t
  end) : Functor.Functor with type 'a t = 'a TupleSection(E).t = struct
  include Functor.MakeFunctor (struct
      include TupleSection (E)

      let fmap f (e, a) = e, f a
      let ( <$ ) a (e, _) = e, a
    end)
end
