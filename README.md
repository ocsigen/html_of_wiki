# html_of_wiki

`html_of_wiki` is a static website generator used by the Ocsigen project to
manage its online documentation. You can find it at https://ocsigen.org.

It is a powerful, language independant, wikicréole compiler, able to manage the
documentation of several versions of multiple projects---all with a single
command line instruction! Its simplicity helps a lot for integrating this tool
in a CI/CD process.

Wikicréole is a wiki-like language that supports **extensions** written in
**OCaml**, a robut and powerful functionnal programming language. It gives the
markup language (almost :wink:) expressivity as HTML!

## Installation

Requires OCaml 4.05 or greater (use `opam switch`) and the `opam` package
manager.

```bash
opam pin add html_of_wiki.dev https://github.com/ocsigen/html_of_wiki.git
```

## Documentation & tutorial

https://ocsigen.org/html_of_wiki/2.0/manual/intro
