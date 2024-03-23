module Op = struct
  type ('a, 'b) t = Op of ('a -> 'b)
end

module Category : Category.Category with type ('a, 'b) t = ('a, 'b) Op.t = struct
  include Op

  let id = Op (fun x -> x)
  let ( @. ) (Op g) (Op f) = Op (fun x -> g (f x))
end
