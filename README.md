# Ocsigen documentation compiler

## Build instructions

```shell
opam pin add html_of_wiki .
```

Beware: since `style.css` is included by a ppx, `src/compiler.ml` won't be
updated after you edit the stylesheet, and the resulting program will still
contain the old one.


## How to update the docs

First, update the project's documentation by issuing this:
```shell
cd project
make doc
```

Then, generate the HTML files using html_of_wiki (`how`):
```shell
cd ../ocsigen.org-data
how-clone
how index.wiki
how-push
```
