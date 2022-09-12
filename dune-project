(lang dune 3.4)

(name utf8_stream)

(generate_opam_files true)

(source
 (github QAhell/utf8_stream_ocaml))

(authors "Armin Heller")

(maintainers "Armin Heller")

(license "Apache-2.0")

(documentation https://github.com/QAhell/utf8_stream_ocaml)

(implicit_transitive_deps false)

(package
 (name utf8_stream)
 (version 0.1)
 (synopsis "UTF-8 encoding and decoding")
 (description "A library for encoding and decoding UTF-8 byte streams in the OCaml programming language")
 (depends ocaml dune)
 (tags (utf8)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
