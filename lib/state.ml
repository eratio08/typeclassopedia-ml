(** {{:https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html} State Monad} *)
module State
    (S : sig
       type t
     end)
    (M : Monad.Monad) =
struct
  (* TODO: Understand the State monad. *)
  type 'a t = S.t -> ('a * S.t) M.t
end
