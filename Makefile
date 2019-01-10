all: ohow wit

ohow:
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc ${CFLAGS}" src/ohow.byte

wit:
	ocamlbuild -use-ocamlfind -ocamlc "ocamlc ${CFLAGS}" src/wit.byte

.PHONY: linkchecker2json
linkchecker2json:
	sbcl --load src/linkchecker2json.lisp <<< "(sb-ext:save-lisp-and-die \"linkchecker2json\" :toplevel #'main :executable t)"

clean:
	ocamlbuild -clean
