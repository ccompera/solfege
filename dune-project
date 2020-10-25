(lang dune 2.4)
(generate_opam_files true)

(name solfege)
(source (github ccompera/solfege_ocaml))
(license MIT)
(authors "ccompera")
(maintainers "ccompera")

(package
 (name solfege)
 (synopsis "Learn your intervals")
 (description "Learn your musical intervals")
 (depends
  (dune (>= 2.4))
  ppx_deriving
  lwt
  js_of_ocaml
  js_of_ocaml-compiler
  js_of_ocaml-ppx
  js_of_ocaml-lwt))
