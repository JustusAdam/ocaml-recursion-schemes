type ('a, 'b) either = Left of 'a | Right of 'b

module type Functor = sig
  type 'a f

  val map : ('a -> 'b) -> 'a f -> 'b f
end

module type Project = sig
  module Base : Functor

  type t

  val project : t -> t Base.f
end

module type Embed = sig
  type t

  module Base : Functor

  val embed : t Base.f -> t
end

module Recursive (M : Project) = struct
  include M

  let rec cata (alg : 'a Base.f -> 'a) (t : t) : 'a =
    alg @@ Base.map (cata alg) @@ project t

  let rec para (alg : (t * 'a) Base.f -> 'a) (t : t) : 'a =
    alg @@ Base.map (fun t -> (t, para alg t)) @@ project t
end

module Corecursive (M : Embed) = struct
  include M

  let rec ana (coal : 'a -> 'a Base.f) (seed : 'a) : t =
    embed @@ Base.map (ana coal) @@ coal seed

  let rec apo (coal : 'a -> (t, 'a) either Base.f) (seed : 'a) : t =
    embed
    @@ Base.map (function Left t -> t | Right a -> apo coal a)
    @@ coal seed
end

module ListF (Elem : sig
  type t
end) =
struct
  type t = Elem.t list

  module Base = struct
    type 'a f = Nil | Cons of Elem.t * 'a

    let map f = function Nil -> Nil | Cons (e, b) -> Cons (e, f b)
  end

  let project = function [] -> Base.Nil | x :: xs -> Base.Cons (x, xs)

  let embed = function Base.Nil -> [] | Base.Cons (x, xs) -> x :: xs
end

module Fix (E : Functor) = struct
  module Base = E

  type t = Fix of t Base.f

  let project (Fix t) = t

  let embed t = Fix t
end

module RecList (E : sig
  type t
end) =
struct
  include Recursive (ListF (E))
end
