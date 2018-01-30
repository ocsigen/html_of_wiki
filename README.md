# Ocsigen documentation compiler

## What is it?

Html_of_wiki makes it possible to update the new ocsigen.org Web Site
on Github from wiki documentation of each project.

It takes the Wiki documentation of each project from repository
`ocsigen.org-data` (which are themselves taken from each project repository).

It translates the wiki into html and saves the html in branch `gh-pages`
of each project, and pushes this branch to github to make it available
online.


## Build instructions

```shell
opam pin add html_of_wiki .
```

Beware: since `style.css` is included by a ppx, `src/compiler.ml` won't be
updated after you edit the stylesheet, and the resulting program will still
contain the old one.

## Doc

```shell
how --help
```
gives command line options.

## How to update the docs

Generate the HTML files using html_of_wiki (`how`):
```shell
cd ../ocsigen.org-data
how-clone
how index.wiki
how-push
```

`how-clone` will clone or update branch gh-pages of all repositories in
`../ocsigen.org-repositories`.

`how index.wiki` will generate the html documentation from wiki files
accessible from `index.wiki`, and wiki files in
`../ocsigen.org-data`. The results is saved in branch `gh-pages`
of each repository.

`how-push` pushes all changes on Github.

## Main pages

The site main pages are hosted as organization github pages.
The wiki sources are in repository `ocsigen.org`
and then copied into directory `ocsigen.github.io`
of repository `ocsigen.org-data`.
The generated html files are in repository `ocsigen.github.io`
(branch `master`).
