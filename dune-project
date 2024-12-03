(lang dune 3.16)

(name aoc2024)

(generate_opam_files true)

(source
 (github stathopulos/aoc2024))

(authors "Konstantinos Stathopulos")

(maintainers "Konstantinos Stathopulos")

(license MIT)

(package
 (name aoc2024)
 (synopsis "Advent of Code Solutions in OCaml")
 (description
  "My solutions for 2024's Advent Of Code challenges using OCaml 5")
 (depends
  (ocaml
   (>= "5"))
  dune)
 (tags
  (aoc ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
