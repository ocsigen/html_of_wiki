(lang dune 3.18)

(using menhir 3.0)
(implicit_transitive_deps false)

(maintainers "The ocsigen team <dev@ocsigen.org>" "Leo Valais <leo.valais97@gmail.com>")
(authors "The ocsigen team <dev@ocsigen.org>")
(license "LGPL-2.1 with OCaml linking exception")
(source (github ocsigen/html_of_wiki))
(documentation "https://ocsigen.org/html_of_wiki/2.0/manual/intro")

(generate_opam_files true)

(name html_of_wiki)

(package
 (name html_of_wiki)
 (version 0.1~beta)
 (synopsis "A wikicreole to HTML compiler")
 (description "a static website generator for software projects, using wikicreole syntax.")
 (depends
  (ocaml (< 5.1))
  ("cmdliner" (>= 1.1.1))
  "js_of_ocaml-ppx_deriving_json"
  "js_of_ocaml-ppx"
  "ocamlfind"
  "re"
  "base64"
  "reason"
  ("tyxml"(>= 4.6.0))))
