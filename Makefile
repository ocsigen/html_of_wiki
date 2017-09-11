all:
	ocamlbuild -use-ocamlfind src/wiki_syntax.byte
clean:
	ocamlbuild -clean
