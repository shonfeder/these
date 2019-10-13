type ('a, 'b) t =
  | This of 'a
  | That of 'b
  | These of 'a * 'b


val equal   : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
val show : (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> ('a, 'b) t -> string

(** {1} Constructors *)

val these : 'a -> 'b -> ('a, 'b) t
val this  : 'a -> ('a, 'b) t
val that  : 'b -> ('a, 'b) t

(** {1} Eliminators *)

val case : ('a -> 'b) -> ('c -> 'b) -> ('a -> 'c -> 'b) -> ('a, 'c) t -> 'b

val to_pair : 'a -> 'b -> ('a, 'b) t -> 'a * 'b

val merge : f:('a -> 'a -> 'a) -> ('a, 'a) t -> 'a
val merge_with : a:('a -> 'c) -> b:('b -> 'c) -> f:('c -> 'c -> 'c) -> ('a, 'b) t -> 'c

(** {1} Partitioning *)

val partition : ('a, 'b) t list -> ('a list * 'b list * ('a * 'b) list)
val partition_here_there : ('a, 'b) t list -> ('a list * 'b list)

(** {1} Distributivity *)

val distr_these_pair : ('a * 'b, 'c) t -> ('a, 'c) t * ('b, 'c) t
val undistr_these_pair : ('a, 'c) t * ('b, 'c) t -> ('a * 'b, 'c) t

val distr_pair_these : (('a, 'b) t * 'c) -> ('a * 'c, 'b * 'c) t
val undsitr_pair_these : ('a * 'c, 'b * 'c) t -> (('a, 'b) t * 'c)

module Make : sig

  module Semigroup (This : Alg.Semigroup.S) (That : Alg.Semigroup.S) :
    Alg.Semigroup.S with type t = (This.t, That.t) t

  module This : sig
    module Functor (T : Alg.Triv.S) : Alg.Functor.S with type 'a t = ('a, T.t) t
    module Cata (T : Alg.Triv.S) : Alg.Cata.S with type 'a t = ('a, T.t) t
  end

  module That : sig
    module Functor (T : Alg.Triv.S) : Alg.Functor.S with type 'b t = (T.t, 'b) t
    module Cata (T : Alg.Triv.S) : Alg.Cata.S with type 'a t = (T.t, 'a) t
  end
end

(* TODO  *)
val bimap : a:('a -> 'b) -> b:('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
