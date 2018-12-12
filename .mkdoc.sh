#!/bin/bash
set -x

# Requires: wget, git, opam, html_of_wiki, jq, comm, find, awk

wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-ocaml.sh
bash -ex .travis-ocaml.sh

eval $(opam env)

git clone https://github.com/ocsigen/html_of_wiki.git
opam pin add -y html_of_wiki html_of_wiki

# wget https://raw.githubusercontent.com/ocsigen/ocsigen.github.io/master/template.wiki 
wget https://raw.githubusercontent.com/ocsigen/ocsigen.github.io/e6b93e987b75be99e8ef30601460f60028c615fd/css/style.css
wget https://raw.githubusercontent.com/ocsigen/ocsigen.github.io/e6b93e987b75be99e8ef30601460f60028c615fd/img/search.svg
mkdir tmp
mv style.css tmp
mv search.svg tmp

f=$(mktemp)
cat >$f <<EOF
{
    "project": "html_of_wiki",
    "api": "api",
    "menu": true,
    "templates": ["tmp/template.wiki"],
    "manual": "manual",
    "assets": "manual/files",
    "images": "manual/files"
}
EOF
DEBUG=t quickdop -f doc _doc -t json -c $f -viu

mv tmp _doc
