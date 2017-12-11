all:
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc -thread" src/client.byte
	js_of_ocaml client.byte
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc -thread ${CFLAGS}" src/html_of_wiki.byte


clean:
	ocamlbuild -clean
