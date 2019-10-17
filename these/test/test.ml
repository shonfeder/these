open QCheck

module Generator = struct
  open These
  let these a b = Gen.oneof
      [ Gen.map this a
      ; Gen.map that b
      ; Gen.map2 these a b
      ]
end

module Arbitrary = struct
  let these a b =
    let show_a fmt a_val = a_val |> Option.get a.print |> Format.fprintf fmt "%s"
    and show_b fmt b_val = b_val |> Option.get b.print |> Format.fprintf fmt "%s"
    in
    make ~print:(These.show show_a show_b) (Generator.these a.gen b.gen)
end

let property name = QCheck.Test.make ~name

let suite name tests =
  let tests = List.map QCheck_alcotest.to_alcotest tests in
  (name, tests)

let property_tests = suite "property tests" [
    property "merging combines [these] with the given function" (pair int int)
      begin fun (a, b) ->
        let open These in
        let f = (+)
        in
        merge ~f (these a b) = a + b
        &&
        merge ~f (this a) = a
        &&
        merge ~f (that b) = b
      end;

    property "[case] is equivalent to case analysis" Arbitrary.(these int string)
      begin fun t ->
        let this_f a = Int.to_string a ^ "'" in
        let that_f b = b ^ "'" in
        let these_f a b = this_f a ^ that_f b in
        let actual = These.case this_f that_f these_f t
        in
        let expected =
          let open These in
          match t with
          | This a     -> this_f a
          | That b     -> that_f b
          | These(a,b) -> these_f a b
        in
        String.equal actual expected
      end;
  ]

let () =
  Alcotest.run "these tests"
    [
      property_tests
    ]
