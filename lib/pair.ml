module Pair = struct
  type 'a t = Pair of 'a * 'a
end

module Functor : Functor.Functor with type 'a t = 'a Pair.t = struct
  include Functor.MakeFunctor (struct
      include Pair

      let fmap f (Pair (a1, a2)) = Pair (f a1, f a2)
      let ( <$ ) a _ = Pair (a, a)
    end)
end
