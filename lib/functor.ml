(** Implements {{:https://wiki.haskell.org/Typeclassopedia#Functor} a Functor}. *)
module type Functor = sig
  type 'a t

  (** Applies the given function with the value of the given functor. *)
  val fmap : 'a 'b. ('a -> 'b) -> 'a t -> 'b t

  (** Lifts the first argument into a functor and ignores the second argument, points towards the value being kept. *)
  val ( <$ ) : 'a 'b. 'a -> 'b t -> 'a t

  (** Infix analog of [fmap] *)
  val ( <$> ) : 'a 'b. ('a -> 'b) -> 'a t -> 'b t

  (** The flipped version of [( <$ )], points towards the value being kept. *)
  val ( $> ) : 'a 'b. 'a t -> 'b -> 'b t

  (** Ignores the value. *)
  val void : 'a. 'a t -> unit t
end

module MakeFunctor (F : sig
    type 'a t

    val fmap : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
    val ( <$ ) : 'a 'b. 'a -> 'b t -> 'a t
  end) : Functor with type 'a t = 'a F.t = struct
  include F

  let ( <$> ) f t = fmap f t

  let ( $> ) t x =
    let flip f x y = f y x in
    flip ( <$ ) t x
  ;;

  let void t = () <$ t
end

module Maybe : Functor with type 'a t = 'a Type.Maybe.t = struct
  include MakeFunctor (struct
      include Type.Maybe

      let fmap f = function
        | Nothing -> Nothing
        | Just a -> Just (f a)
      ;;

      let ( <$ ) a _ = Just a
    end)
end

module Either (E : sig
    type t
  end) : Functor with type 'a t = 'a Type.Either(E).t = struct
  include MakeFunctor (struct
      include Type.Either (E)

      let fmap f = function
        | Right a -> Right (f a)
        | Left _ as left -> left
      ;;

      let ( <$ ) a _ = Right a
    end)
end

module Pair : Functor with type 'a t = 'a Type.Pair.t = struct
  include MakeFunctor (struct
      include Type.Pair

      let fmap f (Pair (a1, a2)) = Pair (f a1, f a2)
      let ( <$ ) a _ = Pair (a, a)
    end)
end

module TupleSection (E : sig
    type t
  end) : Functor with type 'a t = 'a Type.TupleSection(E).t = struct
  include MakeFunctor (struct
      include Type.TupleSection (E)

      let fmap f (e, a) = e, f a
      let ( <$ ) a (e, _) = e, a
    end)
end

module ITree : Functor with type 'a t = 'a Type.ITree.t = struct
  include MakeFunctor (struct
      include Type.ITree

      let rec fmap f = function
        | Leaf g -> Leaf (fun i -> g i |> f)
        | Node xs -> Node (List.map (fun x -> fmap f x) xs)
      ;;

      let ( <$ ) a _ = Leaf (fun _ -> a)
    end)
end
