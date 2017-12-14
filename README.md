# Ocsigen documentation compiler

## Build instructions

```shell
opam pin add html_of_wiki .
```

Beware: since `style.css` is included by a ppx, `src/compiler.ml` won't be
updated after you edit the stylesheet, and the resulting program will still
contain the old one.


## How to update the docs

```shell
cd ../ocsigen.org-data
make doc
PATH=$PATH:../html_of_wiki
clone.sh
html_of_wiki.byte index.wiki
push.sh
```
