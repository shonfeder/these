module type Seed = sig
  include Alg.Functor.S
  val empty : 'a t
  val align : 'a t -> 'b t -> ('a, 'b) These.t t
end

module type S = sig
  include Seed
  val empty : 'a t
  val align : 'a t -> 'b t -> ('a, 'b) These.t t
  val align_with : f:(('a, 'b) These.t -> 'c) -> 'a t -> 'b t -> 'c t
  val semi_align : m:(module Alg.Semigroup.S with type t = 'm) -> 'm t -> 'm t -> 'm t
  val pad_zip : 'a t -> 'b t -> ('a option * 'b option) t
  val pad_zip_with : f:('a option -> 'b option -> 'c) -> 'a t -> 'b t -> 'c t
  val unalign : ('a, 'b) These.t t -> ('a option t * 'b option t)
end

module Make (Seed : Seed) : S with type 'a t = 'a Seed.t = struct
  include Seed
  let align_with ~f a b = Seed.map ~f (align a b)

  let semi_align (type a) ~m:(module S : Alg.Semigroup.S with type t = a) a b =
    align_with ~f:(These.merge ~f:S.op) a b

  let pad_zip a b =
    let f x =
      x
      |> These.bimap ~this:Option.some ~that:Option.some
      |> These.to_pair None None
    in
    align_with ~f a b

  let pad_zip_with ~f a b =
    let f = fun (x, y) -> f x y in
    pad_zip a b |> map ~f

  let unalign t =
    let left  x = These.case Option.some (Fun.const None) (fun a _ -> Some a) x
    and right x = These.case (Fun.const None) Option.some (fun _ b -> Some b) x
    in
    (map ~f:left t, map ~f:right t)
end

module Option : S with type 'a t = 'a Option.t = struct
  module Seed = struct
    include Alg.Functor.Option

    let empty = None

    let align a b = let open These in
      match a  , b with
      | None   , None   -> None
      | Some a , None   -> Some (This a)
      | None   , Some b -> Some (That b)
      | Some a , Some b -> Some (These (a, b))
  end

  include Make (Seed)
end

module List : S with type 'a t = 'a List.t = struct
  module Seed = struct
    include Alg.Functor.List

    let empty = []

    let rec align a b = let open These in
      match a , b with
      | xs    , []    -> map ~f:this xs
      | []    , ys    -> map ~f:that ys
      | x::xs , y::ys -> these x y :: align xs ys
  end

  include Make (Seed)
end

module Seq : S with type 'a t = 'a Seq.t = struct
  module Seed = struct
    include Seq
    let map ~f = map f

    let rec align a b = let open These in
      match a ()    , b () with
      | xs          , Seq.Nil      -> map ~f:this (fun () -> xs)
      | Seq.Nil     , ys           -> map ~f:that (fun () -> ys)
      | Cons (x, xs), Cons (y, ys) -> fun () -> Cons (these x y, align xs ys)
  end

  include Make (Seed)
end

module Map (M : Map.S): S with type 'a t = 'a M.t = struct
  module Seed = struct
    type 'a t = 'a M.t

    let map ~f = M.map f

    let empty = M.empty

    let align a b = let open These in
      let f _ a b =
        match a  , b with
        | None   , None   -> None
        | Some a , None   -> Some (this a)
        | None   , Some b -> Some (that b)
        | Some a , Some b -> Some (these a b)
      in
      M.merge f a b
  end

  include Make (Seed)
end

(* module Stream : S with type 'a t = 'a Stream.t = struct
 *   module Seed = struct
 *     include Alg.Functor.Stream
 *
 *     (\* TODO This won't type check, because the type insists on being weak :( *\)
 *     let empty : _ Stream.t = Stream.from (fun _ -> None)
 *
 *     let next s = try Some (Stream.next s) with Stream.Failure -> None
 *
 *     let align a b =
 *       let f _ = let open These in
 *         match next a, next b with
 *         | None, None -> None
 *         | Some a, None -> Some (This a)
 *         | None, Some b -> Some (That b)
 *         | Some a, Some b -> Some (these a b)
 *       in
 *       Stream.from f
 *   end
 *
 *   include Make (Seed)
 * end *)

module Array : S with type 'a t = 'a Array.t = struct
  module Seed = struct
    include Alg.Functor.Array
    let empty = [||]
    let align a b = Seq.align (Array.to_seq a) (Array.to_seq b) |> Array.of_seq
  end

  include Make (Seed)
end

(* TODO Hashtbl *)
