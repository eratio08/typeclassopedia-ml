(** {{:https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html} Reader Monad} *)
module Reader (M : Monad.Monad) = struct
  (* TODO: Understand the Reader monad. *)
  type ('r, 'a) t = 'r -> 'a M.t
end
