module type S = sig
  include Alg.Functor.S
  include Alg.Cata.S with type 'a t := 'a t

  module Align : Align_internal.S
  type 'a f = 'a Align.t

  val walk : f:('a -> 'b Align.t) -> 'a t -> 'b t Align.t
  val sequence : 'a Align.t t -> 'a t Align.t
end

module type Seed = sig
  include Alg.Functor.S
  include Alg.Cata.S with type 'a t := 'a t

  module Align : Align_internal.S
  type 'a f = 'a Align.t

  val walk : f:('a -> 'b Align.t) -> 'a t -> 'b t Align.t
end

module Make (Seed : Seed) : S
  with module Align = Seed.Align
   and type 'a t = 'a Seed.t
   and type 'a f = 'a Seed.Align.t
