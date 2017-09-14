all:
	ocamlbuild -use-ocamlfind src/html_of_wiki.byte
clean:
	ocamlbuild -clean
