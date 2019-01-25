all:
	dune build src/wit/wit.exe src/ohow/ohow.exe

.PHONY: linkchecker2json
linkchecker2json:
	sbcl --load src/linkchecker2json.lisp <<< "(sb-ext:save-lisp-and-die \"linkchecker2json\" :toplevel #'main :executable t)"

clean:
	dune clean
