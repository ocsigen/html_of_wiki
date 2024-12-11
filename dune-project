(lang dune 3.17)
(name html_of_wiki)

(generate_opam_files true)
(implicit_transitive_deps false)

(license "LGPL-2.1 with OCaml linking exception")
(authors "The ocsigen team <dev@ocsigen.org>")
(maintainers "The ocsigen team <dev@ocsigen.org>" "Leo Valais <leo.valais97@gmail.com>")
(source (github ocsigen/html_of_wiki))
(documentation "https://ocsigen.org/html_of_wiki/2.0/manual/intro")

(package
 (name html_of_wiki)
 (synopsis "A wikicreole to HTML compiler")
 (description "a static website generator for software projects, using wikicreole syntax.")
 (depends
  (ocaml (< 5.0))
  ("cmdliner" (>= 1.1.1))
  ("tyxml"(>= 4.6.0))
  "base64"
  "js_of_ocaml-ppx"
  "js_of_ocaml-ppx_deriving_json"
  "ocamlfind"
  "re"
  "reason"))
