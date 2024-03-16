module ITree = struct
  type 'a t =
    | Leaf of (int -> 'a)
    | Node of 'a t list
end

module Functor : Functor.Functor with type 'a t = 'a ITree.t = struct
  include Functor.MakeFunctor (struct
      include ITree

      let rec fmap f = function
        | Leaf g -> Leaf (fun i -> g i |> f)
        | Node xs -> Node (List.map (fun x -> fmap f x) xs)
      ;;

      let ( <$ ) a _ = Leaf (fun _ -> a)
    end)
end
