
(** {{:https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Cont.html} Continuation Monad} *)
module Cont = struct
  (* TODO: Understand the Continuation monad. *)
  type ('r, 'a) t = Cont of (('a -> 'r) -> 'r)
end
