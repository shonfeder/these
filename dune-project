(lang dune 1.11)
(name these)

(generate_opam_files true)

(authors "Shon Feder")
(maintainers "shon.feder@gmail.com")

(package
 (name these)
 (synopsis "Port of Haskell's \"Data.These\" and associated auxiliaries")
 (description "Based on the package authored by C. McCann, Oleg Grenrus")
 (depends
  (ppx_deriving (>= 4.4))
  (alg_structs (>= 0.1.3))

  (dune (and :build (>= 1.11.3)))

  (qcheck (and :with-test (>= 0.11)))
  (qcheck-alcotest (and :with-test (>= 0.11)))
  (alcotest (and :with-test (>= 0.8.5)))
))

(package
 (name these.align)
 (synopsis "Port of Haskell's \"Data.Align\" and associated auxiliaries")
 (description "Based on the package authored by C. McCann, Oleg Grenrus")
 (depends
  (alg_structs (>= 0.1.3))
  (these (>= ~dev))

  (dune (and :build (>= 1.11.3)))

  (qcheck (and :with-test (>= 0.11)))
  (qcheck-alcotest (and :with-test (>= 0.11)))
  (alcotest (and :with-test (>= 0.8.5)))
))
