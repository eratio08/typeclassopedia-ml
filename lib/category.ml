module type Category = sig
  type ('a, 'b) t

  val id : ('a, 'a) t
  val ( @. ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
end
