(lang dune 2.4)
(generate_opam_files true)

(name solfege)
(source (github ccompera/solfege_ocaml))
(license MIT)
(authors "ccompera")

(package
 (name solfege)
 (synopsis "Learn your intervals")
 (description "Learn your musical intervals")
 (depends
  (dune (>= 2.4))
  ppx_deriving
  cmdliner))
