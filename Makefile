all:
	dune build src/wit/wit.exe src/ohow/ohow.exe

fmt:
	dune build @fmt --auto-promote

clean:
	dune clean

.PHONY: linkchecker2json all fmt
