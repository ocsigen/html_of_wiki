(lang dune 2.0)

(using menhir 2.0)

(name html_of_wiki)

(implicit_transitive_deps false)

(license "LGPL-2.1 with OCaml linking exception")

(authors "The ocsigen team <dev@ocsigen.org>")

(maintainers
 "The ocsigen team <dev@ocsigen.org>"
 "Leo Valais <leo.valais97@gmail.com>")

(source
 (github ocsigen/html_of_wiki))

(bug_reports "https://github.com/ocsigen/html_of_wiki/issues")

(homepage "https://github.com/ocsigen/html_of_wiki")

(documentation "https://ocsigen.org/html_of_wiki/2.0/manual/intro")

(generate_opam_files true)

(package
 (name html_of_wiki)
 (synopsis "A wikicreole to HTML compiler")
 (description
  "a static website generator for software projects, using wikicreole syntax.")
 (depends
  ("cmdliner" (>= 1.1.1))
  "js_of_ocaml-ppx_deriving_json"
  "js_of_ocaml-ppx"
  "ocamlfind"
  "re"
  "base64"
  "reason"
  ("tyxml"(>= 4.6.0))))
