all:
	dune build src/wit/wit.exe src/ohow/ohow.exe

client:
	dune build src/client/client.js

fmt:
	dune build @fmt --auto-promote

clean:
	dune clean

.PHONY: linkchecker2json all fmt
