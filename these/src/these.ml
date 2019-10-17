type ('a, 'b) t =
  | This of 'a
  | That of 'b
  | These of 'a * 'b
[@@deriving eq, ord, show]

let to_string fa fb t =
  let show' f fmt x = x |> f |> Format.fprintf fmt "%s" in
  show (show' fa) (show' fb) t

(** Constructors *)

let these a b = These (a, b)
let this a = This a
let that b = That b

let map_this ~f = function
  | This x       -> This (f x)
  | That y       -> That y
  | These (x, y) -> These (f x, y)

let map_that ~f = function
  | This x       -> This x
  | That y       -> That (f y)
  | These (x, y) -> These (x, f y)

(** Eliminators *)

let case l r lr = function
  | This a       -> l a
  | That a       -> r a
  | These (a, b) -> lr a b

let to_pair a b = function
  | This a       -> (a, b)
  | That b       -> (a, b)
  | These (a, b) -> (a, b)

let merge ~f = case Fun.id Fun.id f

let bimap ~this ~that = function
  | This a       -> This (this a)
  | That b       -> That (that b)
  | These (a, b) -> These (this a, that b)

let merge_with ~this ~that ~these t = t |> bimap ~this ~that |> merge ~f:these

(* TODO Make tail recursive *)
let rec partition = function
  | [] -> ([], [], [])
  | (x :: xs) ->
    let (as', bs', abs') = partition xs in
    match x with
    | This a -> (a :: as', bs', abs')
    | That b -> (as', b :: bs', abs')
    | These (a, b) -> (as', bs', (a, b) :: abs')

(* TODO rename removing redundnat "these" etc *)

let rec partition_here_there = function
  | [] -> ([], [])
  | (x :: xs) ->
    let (as', bs') = partition_here_there xs in
    match x with
    | This a       -> (a :: as', bs')
    | That b       -> (as', b :: bs')
    | These (a, b) -> (a :: as', b :: bs')

(* TODO partition non-empty? *)
let distr_these_pair = function
  | This ((a, b))     -> (This a, This b)
  | That c            -> (That c, That c)
  | These ((a, b), c) -> (These (a, c), These (b, c))

let undistr_these_pair : (('a, 'c) t * ('b, 'c) t) -> ('a * 'b, 'c) t = function
  | (This a      , This b)       -> This ((a, b))
  | (That c      , That _)       -> That c
  | (These (a, c), These (b, _)) -> These ((a, b), c)
  | (This _      , That c)       -> That c
  | (This a      , These (b, c)) -> These ((a, b), c)
  | (That c      , This _)       -> That c
  | (That c      , These _)      -> That c
  | (These (a, c), This b)       -> These ((a, b), c)
  | (These (_, c), That _)       -> That c

let distr_pair_these = function
  | This a, c       -> This (a, c)
  | That b, c       -> That (b, c)
  | These (a, b), c -> These ((a, c), (b, c))

let undsitr_pair_these = function
  | This ((a, c))          -> (This a, c)
  | That ((b, c))          -> (That b, c)
  | These ((a, c), (b, _)) -> (These (a, b), c)

module Make = struct
  module Semigroup (This : Alg.Semigroup.S) (That : Alg.Semigroup.S) :
    Alg.Semigroup.S with type t = (This.t, That.t) t = struct
    include Alg.Semigroup.Make (struct
        type nonrec t = (This.t, That.t) t
        let op a b = match a, b with
          | This   a    , This   b     -> This (This.op a b)
          | This   a    , That      y  -> These (a, y)
          | This   a    , These (b, y) -> These (This.op a b, y)
          | That      x , This   b     -> These (b, x)
          | That      x , That      y  -> That (That.op x y)
          | That      x , These (b, y) -> These (b,  (That.op x y))
          | These (a, x), This   b     -> These (This.op a b, x)
          | These (a, x), That      y  -> These (a, That.op x y)
          | These (a, x), These (b, y) -> These (This.op a b, That.op x y)
      end)
  end

  module This = struct
    module Functor (T : Alg.Triv.S) : Alg.Functor.S with type 'a t = ('a, T.t) t = struct
      type nonrec 'a t = ('a, T.t) t
      let map = map_this
    end

    module Cata (T : Alg.Triv.S) : Alg.Cata.S with type 'a t = ('a, T.t) t = struct
      module Seed = struct
        type nonrec 'a t = ('a, T.t) t

        let fold_right ~f t ~init = match t with
          | This x -> f x init
          | That _ -> init
          | These (x, _) -> f x init
      end

      include Alg.Cata.Make(Seed)
    end
  end (* THis *)

  module That = struct
    module Functor (T : Alg.Triv.S) : Alg.Functor.S with type 'b t = (T.t, 'b) t = struct
      type nonrec 'b t = (T.t, 'b) t
      let map = map_that
    end

    module Cata (T : Alg.Triv.S) : Alg.Cata.S with type 'a t = (T.t, 'a) t = struct
      module Seed = struct
        type nonrec 'a t = (T.t, 'a) t

        let fold_right ~f t ~init = match t with
          | This _ -> init
          | That x -> f x init
          | These (_, x) -> f x init
      end

      include Alg.Cata.Make(Seed)
    end

    module Traversable (A : Alg.Applicative.S) (T : Alg.Triv.S) :
      Alg.Traversable.S with type 'a t = (T.t, 'a) t = struct
      module Seed = struct
        include Functor (T)
        include Cata (T)

        module A = A
        type nonrec 'a t = 'a t

        let traverse ~f = function
          | This a -> A.return (This a)
          | That x -> A.map ~f:that (f x)
          | These (a, x) -> A.map ~f:(these a) (f x)
      end

      include Alg.Traversable.Make (Seed)
    end
  end (* That *)

end (* Make *)
