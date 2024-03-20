module Maybe = struct
  type 'a t =
    | Nothing
    | Just of 'a

  let pp pp_a fmt = function
    | Nothing -> Format.fprintf fmt "Nothing@."
    | Just a -> Format.fprintf fmt "Just(%a)@." pp_a a
  ;;
end

module Functor : Functor.Functor with type 'a t = 'a Maybe.t = Functor.MakeFunctor (struct
    include Maybe

    let fmap f = function
      | Nothing -> Nothing
      | Just a -> Just (f a)
    ;;

    let ( <$ ) a _ = Just a
  end)

module Applicative : Applicative.Applicative with type 'a t = 'a Functor.t =
Applicative.MakeApplicative (struct
    include Functor
    open Maybe

    let pure a = Just a
    let empty = Nothing

    let ( <*> ) ft t =
      match ft, t with
      | Just f, Just a -> Just (f a)
      | _, _ -> Nothing
    ;;
  end)

module Monad : Monad.Monad with type 'a t = 'a Applicative.t = Monad.MakeMonad (struct
    include Applicative
    open Maybe

    let ( >>= ) t f =
      match t with
      | Just a -> f a
      | Nothing -> Nothing
    ;;
  end)

module Semigroup (T : sig
    type t

    val ( <> ) : t -> t -> t
  end) : Semigroup.Semigroup with type t = T.t Maybe.t = Semigroup.MakeSemiGroup (struct
    include T
    include Maybe

    type t = T.t Maybe.t

    let ( <> ) a b =
      match a, b with
      | Just a, Just b -> Just (a <> b)
      | Just a, Nothing | Nothing, Just a -> Just a
      | _, _ -> Nothing
    ;;
  end)
