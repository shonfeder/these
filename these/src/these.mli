(** {1 [These]: A Type for Inclusive Disjunction }

    A port and adaptation of C. McCann's and Oleg Grenrus'
    {{:http://hackage.haskell.org/package/these} Data.These} Haskell library.

    {! type:These.t} is a type for inclusive disjunctions. It is used in {! Align}
    to provide structure matching on structures of different sizes, and can be
    leveraged anywhere you need to represent a value which can be {b this} thing
    of type ['a], {b that} thing of type ['b] or both of {b these} things given
    together.
*)

(** {2 The type of [These]} *)

(** Type [('a, 'b) t] represents the inclusive disjunction of types ['a] and
    ['b].

    [('a, 'b) t] is logically equivalent to the type [('a, ('b * 'a Option.t))
    Result.t ], but easier to understand and work with.

*)
type ('a, 'b) t =
  | This of 'a       (** [This a] is this [a : 'a] by itself *)
  | That of 'b       (** [That b] is that [b : 'b] by itself *)
  | These of 'a * 'b (** [These (a, b)] gives both this [a : 'a] and that [b : 'b] *)
(** Given a value [These(a, b) : ('a, 'b) t] we can refer to [a] as {i this} and
    refer to [b] as {i that}. E.g., in [These(1, "a") : (int, string) t] {i
    this} is [1] and {i that} is ["a"]. *)

(** {2 Inspectors}

    Functions for examining and representing values of type {!type: These.t}. *)

(** {3 Comparing}*)

(** [equal equal_a equal_b (These(a, b)) (These(a', b'))] is [true] just in
    case [equal_a a a'] and [equal_b b b'] are both [true]. *)
val equal   : ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

(** [compare compare_a compare_b these_ab these_ab'] is the total ordering over
    [these_ab] and [these_ab'] as determined by the comparison functions
    [compare_a] and [compare_b]. Following the usual OCaml convention, the
    inequality is represented as a signed integer.

    The value constructures are sorted thus [This < That < These] and, e.g.,

    - [compare Int.compare String.compare (These(0, "z")) (These(1, "a"))] is [-1]
    - [compare Int.compare String.compare (That("z")) (These(1, "a"))] is [-1]
    - [compare Int.compare String.compare (This(2)) (That("z"))] is [-1]  *)
val compare : ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int

(** {3 Representing} *)

(** [pp formatter_a formatter_b formatter these_ab] is a pretty printing
    formatter for [these_ab] derived from the formatters [formatter_a]
    [formatter_b] *)
val pp : (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) t ->
  unit

(** [show a_formatter b_formatter these_ab] is a string representing [these_ab]
    according to the string representations produced by the supplied formatters
    for the possible [a] and [b] in [these_ab] *)
val show : (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) -> ('a, 'b) t -> string

(** [to_string a_to_string b_to_string these_ab] is a string representation of
    [these_ab] based on the string representations of its components provided by
    [a_to_string] and [b_to_string]. *)
val to_string : ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string

(** {2 Constructors}

    Functions for creating new values of type {!type:These.t}. *)

(** {3 Constructor Functions} *)

(** [these a b] are [These (a, b)] *)
val these : 'a -> 'b -> ('a, 'b) t

(** [this a] is [This a] *)
val this  : 'a -> ('a, 'b) t

(** [that b] is [That b] *)
val that  : 'b -> ('a, 'b) t

(** [map_this ~f these_ab] is [these_ab] but with [f] applied to the value of
    {i this} *)
val map_this : f:('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t

(** [map_that ~f these_ab] is [these_ab] but with [f] applied to the value of
    {i that} *)
val map_that : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

(** {2 Eliminators}

    Functions for deriving other types from values of type {!type:These.t}. *)

(** [case a_handler b_handler ab_handler these_ab] performs a case analysis on
    [these_ab], dispatching {i this} [a], {i that} [b], or both [a] and [b] to
    the appropriate handler. It is equivalent to a case analysis using a match
    statement. *)
val case : ('a -> 'b) -> ('c -> 'b) -> ('a -> 'c -> 'b) -> ('a, 'c) t -> 'b

(** [to_pair a_default b_default these_ab] is [(a, b)] if [these_ab] is [These
    (a, b)]. Otherwise it is a tuple holding {i this} or {i that} in the
    respective position, with the missing value supplied by the corresponding
    default. *)
val to_pair : 'a -> 'b -> ('a, 'b) t -> 'a * 'b

(** [merge ~f these_ab] is [f a b] if [these_ab] is [These(a, b)], otherwise
    it is just {i this} or {i that}

    @param f a binary function merging both of {i these} *)
val merge : f:('a -> 'a -> 'a) -> ('a, 'a) t -> 'a

(** [merge_with ~this ~that ~these these_ab] is like {!val:merge}, but maps
    {i this} or {i that} to a new value with the respective function if
    needed *)
val merge_with : this:('a -> 'c) -> that:('b -> 'c) -> these:('c -> 'c -> 'c) -> ('a, 'b) t -> 'c

(** {2 Partitioning} *)

(** TODO *)
val partition : ('a, 'b) t list -> ('a list * 'b list * ('a * 'b) list)

(** TODO *)
val partition_here_there : ('a, 'b) t list -> ('a list * 'b list)

(** {2 Distributivity} *)

(** TODO *)
val distr_these_pair : ('a * 'b, 'c) t -> ('a, 'c) t * ('b, 'c) t

(** TODO *)
val undistr_these_pair : ('a, 'c) t * ('b, 'c) t -> ('a * 'b, 'c) t

(** TODO *)
val distr_pair_these : (('a, 'b) t * 'c) -> ('a * 'c, 'b * 'c) t

(** TODO *)
val undsitr_pair_these : ('a * 'c, 'b * 'c) t -> (('a, 'b) t * 'c)

(** {2 Algebraic structures}

    The modules in {!module:Make} define module functors to construct various
    algebraic structures for types of {!type:These}.
*)

(** The algebraic structures available from [Make] are divided into 3 sorts: {i
    unbiased structures}, {i this biased structure}s and {i that biased
    structures}.

    {i Unbiased structures} weight {i this} and {i that} equally. If an
    operation is applied, or a structure accounted for, it holds equally for {i
    this} and {i that}. {i this biased structures} and {i that biased
    structures} only apply their operations to the respective side of the
    disjunction.

    {3 Unbiased Structures}

    {!modules: Make.Semigroup}

    {3 {b This} Biased Structures}

    {!modules: Make.This.Functor  Make.This.Cata}

    {3 {b That} Biased Structures}

    {!modules: Make.That.Functor  Make.That.Cata} *)
module Make : sig

  (** TODO {!module-type:Alg.Semigroup.S} balh blah yaya yeaaa sbinginf

      dings dongs tongs dude yep *)
  module Semigroup (This : Alg.Semigroup.S) (That : Alg.Semigroup.S) :
    Alg.Semigroup.S with type t = (This.t, That.t) t

  (** TODO This and That blah dah

      dinsdf asidnf easd *)
  module This : sig
    module Functor (T : Alg.Triv.S) : Alg.Functor.S with type 'a t = ('a, T.t) t
    module Cata (T : Alg.Triv.S) : Alg.Cata.S with type 'a t = ('a, T.t) t
  end

  (** TODO This and That blah dah *)
  module That : sig
    module Functor (T : Alg.Triv.S) : Alg.Functor.S with type 'b t = (T.t, 'b) t
    module Cata (T : Alg.Triv.S) : Alg.Cata.S with type 'a t = (T.t, 'a) t
  end
end

(** TODO *)
val bimap : this:('a -> 'b) -> that:('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
