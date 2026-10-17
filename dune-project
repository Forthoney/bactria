(lang dune 3.24)

(name bactria)

(implicit_transitive_deps false)

(generate_opam_files true)

(source
 (github Forthoney/bactria))

(authors "Seong-Heon Jung <castlehoneyjung@gmail.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name bactria)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml base core unboxed unboxed_datatypes ppx_template ocaml_simd)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
