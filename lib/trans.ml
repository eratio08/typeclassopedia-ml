(*  TODO: study Monad Transformers. https://wiki.haskell.org/Typeclassopedia#Monad_transformers *)
module type Trans = sig
  module M : Monad.Monad

  type 'a t

  val lift : 'a M.t -> 'a M.t t
end
