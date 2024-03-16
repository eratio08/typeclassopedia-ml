module type Monad = sig
  include Applicative.Applicative

  (** Synonym for [Applicative.pure] lifts the argument into a computational context. *)
  val return : 'a. 'a -> 'a t

  (** Also known as [flatmap] or [bind]. Combines two computations into one bigger computation. *)
  val ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t

  (** OCamls do-notation alternative.
      {[
        let () =
          let* x = Just 2 in
          Just x;
          (* is equivalent of *)
          Just 2 >>= fun x -> x
        ;;
      ]}
      See {{:https://v2.ocaml.org/manual/bindingops.html} Binding Ops} for details. *)
  val ( let* ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t

  (** Is like [Applicative.( *> )]. Keeps the argument it pointing to. *)
  val ( >> ) : 'a 'b. 'a t -> 'b t -> 'b t

  (** Synonym for [Functor.fmap]. *)
  val lift_m : 'a 'b. ('a -> 'b) -> 'a t -> 'b t

  (** Synonym for [Applicative.( <*> )]. *)
  val ap : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t

  (** Like [>>=] but with reversed arguments. *)
  val ( =<< ) : 'a 'b. ('a -> 'b t) -> 'a t -> 'b t

  (** Maps the first argument over the second argument and gathers the results in a list. *)
  val map_m : 'a 'b. ('a -> 'b t) -> 'a list -> 'b list t

  (** Takes a list of computations and collects the results in a list. *)
  val sequence : 'a 'b. 'a t list -> 'a list t

  (** Function combinator. Like Haskell's [( . )] but in the computational context.
      Also know as 'Kleisli-composition operator'. *)
  val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t

  (** Function combinator. The flipped version of [( <=< )]. *)
  val ( <=< ) : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
end

let rec fix f x = f (fix f) x

module MakeMonad (M : sig
    include Applicative.Applicative

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  end) : Monad with type 'a t = 'a M.t = struct
  include M

  let ( let* ) t f = t >>= f
  let return = pure
  let ( =<< ) f t : 'b t = t >>= f
  let ( >> ) t1 t2 = t1 >>= fun _ -> t2
  let lift_m = fmap
  let ap = ( <*> )

  let map_m f =
    let fold_r f l =
      let rec go = function
        | [] -> l
        | y :: ys -> f y (go ys)
      in
      go
    in
    let k a r = f a >>= fun x -> r >>= fun xs -> return (x :: xs) in
    fold_r k (return [])
  ;;

  let sequence x = map_m (fun x -> x) x
  let ( >=> ) f g x = f x >>= g
  let ( <=< ) g f = f >=> g
end
