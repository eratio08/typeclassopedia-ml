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
