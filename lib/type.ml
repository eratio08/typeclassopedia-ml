module Maybe = struct
  type 'a t =
    | Nothing
    | Just of 'a
end

module Either (E : sig
    type t
  end) =
struct
  type 'a t =
    | Left of E.t
    | Right of 'a
end

module Pair = struct
  type 'a t = Pair of 'a * 'a
end

module TupleSection (E : sig
    type t
  end) =
struct
  type 'a t = E.t * 'a
end

module ITree = struct
  type 'a t =
    | Leaf of (int -> 'a)
    | Node of 'a t list
end
