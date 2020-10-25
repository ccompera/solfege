# Solfege

Learn your intervals!

## Install dependencies
`opam install --deps-only -t .`

## ocamlformat
`dune build @fmt`
`dune promote [_files_]`

## Build
`dune build`

## Execute
`dune exec solfege`

## Add a dependency
Add it to the `dune-project` and in `dune`, then `dune build` to generate the `solfege.opam` file.
