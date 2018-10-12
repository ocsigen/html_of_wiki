all:
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc" src/client.byte
	js_of_ocaml +weak.js client.byte
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc ${CFLAGS}" src/html_of_wiki.byte

ohow:
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc ${CFLAGS}" src/ohow.byte

wit:
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc ${CFLAGS}" src/wit.byte

.PHONY: linkchecker2json
linkchecker2json:
	sbcl --load linkchecker2json.lisp <<< "(sb-ext:save-lisp-and-die \"linkchecker2json\" :toplevel #'main :executable t)"

clean:
	ocamlbuild -clean
