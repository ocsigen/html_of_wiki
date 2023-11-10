# html_of_wiki

`html_of_wiki` is a static website generator for software projects.

It is a powerful, language independant, wikicréole compiler, able to manage the
documentation of several versions of multiple projects---all with a single
command line instruction! Its simplicity helps a lot for integrating this tool
in a CI/CD process.

Wikicréole is a wiki-like language that supports **extensions** written in

html_of_wiki can manage API documentation and user manuals for several versions of your software.
It can manage multi-projects sites with sub-projects, and inter-projects links.

## Installation

Requires OCaml 4.08 or greater (use `opam switch`) and the `opam` package
manager.

```bash
opam pin add html_of_wiki.dev https://github.com/ocsigen/html_of_wiki.git
```

## Documentation & tutorial

https://ocsigen.org/html_of_wiki/2.0/manual/intro
