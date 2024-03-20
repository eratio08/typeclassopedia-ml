module Either (E : sig
    type t
  end) =
struct
  type 'a t =
    | Left of E.t
    | Right of 'a
end

module Functor (E : sig
    type t
  end) : Functor.Functor with type 'a t = 'a Either(E).t = struct
  include Functor.MakeFunctor (struct
      include Either (E)

      let fmap f = function
        | Right a -> Right (f a)
        | Left _ as left -> left
      ;;

      let ( <$ ) a _ = Right a
    end)
end

module Applicative (E : sig
    type t

    val empty : t
  end) : Applicative.Applicative with type 'a t = 'a Functor(E).t =
Applicative.MakeApplicative (struct
    include Functor (E)
    open Either (E)

    let pure a = Right a
    let empty = Left E.empty

    let ( <*> ) (f : ('a -> 'b) t) (t : 'a t) : 'b t =
      match f, t with
      | Right f, Right a -> Right (f a)
      | Left f, _ -> Left f
      | _, Left e -> Left e
    ;;
  end)

module Monad (E : sig
    type t

    val empty : t
  end) : Monad.Monad with type 'a t = 'a Applicative(E).t = Monad.MakeMonad (struct
    include Applicative (E)
    open Either (E)

    let ( >>= ) t f =
      match t with
      | Right a -> f a
      | Left e -> Left e
    ;;
  end)
