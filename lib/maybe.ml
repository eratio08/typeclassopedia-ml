module Maybe = struct
  type 'a t =
    | Nothing
    | Just of 'a
end

module Functor : Functor.Functor with type 'a t = 'a Maybe.t = struct
  include Functor.MakeFunctor (struct
      include Maybe

      let fmap f = function
        | Nothing -> Nothing
        | Just a -> Just (f a)
      ;;

      let ( <$ ) a _ = Just a
    end)
end

module Applicative : Applicative.Applicative with type 'a t = 'a Functor.t = struct
  include Applicative.MakeApplicative (struct
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
end

module Monad : Monad.Monad with type 'a t = 'a Applicative.t = struct
  include Monad.MakeMonad (struct
      include Applicative
      open Maybe

      let ( >>= ) t f =
        match t with
        | Just a -> f a
        | Nothing -> Nothing
      ;;
    end)
end
