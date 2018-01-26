# Ocsigen documentation compiler

## What is it?

Html_of_wiki makes it possible to update the new ocsigen.org Web Site
on Github from wiki documentation of each project.

It takes the Wiki documentation of each project from repository
ocsigen.org-data (which are themselves taken from each project repository).

It translates the wiki into html and saves the html in branch gh-pages
of each project, and pushes this branch to github to make it available
online.


## Build instructions

```shell
opam pin add html_of_wiki .
```

Beware: since `style.css` is included by a ppx, `src/compiler.ml` won't be
updated after you edit the stylesheet, and the resulting program will still
contain the old one.

## How to update the docs

Generate the HTML files using html_of_wiki (`how`):
```shell
cd ../ocsigen.org-data
how-clone
how index.wiki
how-push
```
