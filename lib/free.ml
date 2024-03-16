(** {{:https://hackage.haskell.org/package/free-5.2/docs/Control-Monad-Free.html} Free Monad} *)
module Free (F : Functor.Functor) = struct
  (* TODO: Understand the Free monad. *)
  type 'a t =
    | Pure of 'a
    | Free of 'a t F.t
end
