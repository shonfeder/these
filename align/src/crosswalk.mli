module Align = Align_internal

module type S = sig
  include Alg.Functor.S
  include Alg.Cata.S with type 'a t := 'a t

  type 'a f

  val walk : f:('a -> 'b f) -> 'a t -> 'b t f
  val sequence : 'a f t -> 'a t f
end

module type Seed = sig
  include Alg.Functor.S
  include Alg.Cata.S with type 'a t := 'a t

  module Align : Align.S
  type 'a f = 'a Align.t

  val walk : f:('a -> 'b Align.t) -> 'a t -> 'b t Align.t
end

module Make (Seed : Seed) : S with type 'a t = 'a Seed.t
                               and type 'a f = 'a Seed.Align.t

module Option : sig
  module Make (A : Align.S) : S with type 'a t = 'a Option.t
                                 and type 'a f = 'a A.t
end

module List : sig
  module Make (A : Align.S) : S with type 'a t = 'a List.t
                                 and type 'a f = 'a A.t
end
