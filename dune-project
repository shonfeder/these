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
  (alg (>= ~dev))
  (dune (and :build (>= 1.11.3)))))

(package
 (name these.align)
 (synopsis "Port of Haskell's \"Data.Align\" and associated auxiliaries")
 (description "Based on the package authored by C. McCann, Oleg Grenrus")
 (depends
  (alg (>= ~dev))
  (these (>= ~dev))
  (dune (and :build (>= 1.11.3)))))
