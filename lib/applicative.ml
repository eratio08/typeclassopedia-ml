module type Applicative = sig
  type 'a t

  include Functor.Functor with type 'a t := 'a t

  (** Lifts the argument into the computational context, effect free. *)
  val pure : 'a. 'a -> 'a t

  (** Empty computational context. *)
  val empty : 'a t

  (** Chaining operator, like a lifted version of [fmap] in a computational context. *)
  val ( <*> ) : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t

  (** Chaining operator. Keep the side it points to. *)
  val ( *> ) : 'a 'b. 'a t -> 'b t -> 'b t

  (** Chaining operator. Keep the side it points to. *)
  val ( <* ) : 'a 'b. 'a t -> 'b t -> 'a t

  (** Has the same signature of [fmap]. It mainly exists to parallel [lift_a_2] and [lift_a_3]. *)
  val lift_a : 'a 'b. ('a -> 'b) -> 'a t -> 'b t

  (** Lifts a two argument function into the computational context. *)
  val lift_a_2 : 'a 'b 'c. ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  (** Lifts a three argument function into the computational context. *)
  val lift_a_3 : 'a 'b 'c 'd. ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  (** Behaves like [<*>]. The results of the first computation are provided as input to the function of the second computation.
      The signature might look like this is a case of [flip (<*>)] but this operator keep the order of operations. *)
  val ( <**> ) : 'a 'b. 'a t -> ('a -> 'b) t -> 'b t

  (** Conditionally executes a computation.
      If the first argument is [true] the second computation is evaluate, otherwise the last computation is evaluated. *)
  val when_ : bool -> unit t -> unit t

  (** Behaves like [when_] but negated. *)
  val unless : bool -> unit t -> unit t

  (** Evaluate only of the first argument is [true]. *)
  val guard : bool -> unit t
end

(** Helper module to provide default implementations of applicative functions. *)
module MakeApplicative (A : sig
    include Functor.Functor

    val pure : 'a -> 'a t
    val empty : 'a t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  end) : Applicative with type 'a t = 'a A.t = struct
  include A

  let const (x : 'a) (_ : 'b) : 'a = x
  let id a = a
  let ( *> ) ft t = id <$ ft <*> t
  let lift_a f t = pure f <*> t
  let lift_a_2 f t = ( <*> ) (fmap f t)
  let lift_a_3 f t1 t2 t3 = lift_a_2 f t1 t2 <*> t3
  let ( <* ) t1 t2 = lift_a_2 const t1 t2
  let ( <**> ) t f = f <*> t
  let when_ p s = if p then s else pure ()
  let unless p s = if not p then s else pure ()
  let guard p = if p then pure () else empty
end

module Maybe : Applicative with type 'a t = 'a Functor.Maybe.t = struct
  include MakeApplicative (struct
      include Functor.Maybe
      open Type.Maybe

      let pure a = Just a
      let empty = Nothing

      let ( <*> ) ft t =
        match ft, t with
        | Just f, Just a -> Just (f a)
        | _, _ -> Nothing
      ;;
    end)
end

module Either (E : sig
    type t

    val empty : t
  end) : Applicative with type 'a t = 'a Functor.Either(E).t = struct
  include MakeApplicative (struct
      include Functor.Either (E)
      open Type.Either (E)

      let pure a = Right a
      let empty = Left E.empty

      let ( <*> ) (f : ('a -> 'b) t) (t : 'a t) : 'b t =
        match f, t with
        | Right f, Right a -> Right (f a)
        | Left f, _ -> Left f
        | _, Left e -> Left e
      ;;
    end)
end
