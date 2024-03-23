module Kleisli (M : Monad.Monad) = struct
  type ('a, 'b) t = Kleisli of ('a -> 'b M.t)
end

module Category (M : Monad.Monad) :
  Category.Category with type ('a, 'b) t = ('a, 'b) Kleisli(M).t = struct
  include Kleisli (M)

  let id = Kleisli M.return

  let ( @. ) (Kleisli g) (Kleisli f) =
    let open M in
    Kleisli (f >=> g)
  ;;
end
