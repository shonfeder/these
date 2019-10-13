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

module Make (Seed : Seed) = struct
  include Seed

  let sequence t = Seed.walk ~f:Fun.id t
end

module Option = struct
  module Make (A : Align.S) = struct
    module Seed = struct
      include Alg.Functor.Option
      include Alg.Cata.Option

      module Align = A
      type 'a f = 'a Align.t

      let walk ~f = function
        | None -> A.empty
        | Some a -> A.map ~f:Option.some (f a)
    end

    include Make (Seed)
  end
end

module List = struct
  module Make (A : Align.S) = struct
    module Seed = struct
      include Alg.Functor.List
      include Alg.Cata.List

      module Align = A
      type 'a f = 'a Align.t

      let rec walk ~f t =
        let cons_these th = These.case (fun x -> [x]) Fun.id (fun x xs -> x :: xs) th in
        match t with
        | []      -> Align.empty
        | x :: xs -> Align.align_with ~f:cons_these (f x) (walk ~f xs)
    end

    include Make (Seed)
  end
end
