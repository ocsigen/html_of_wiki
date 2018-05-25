all:
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc" src/client.byte
	js_of_ocaml +weak.js client.byte
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc ${CFLAGS}" src/html_of_wiki.byte


clean:
	ocamlbuild -clean
