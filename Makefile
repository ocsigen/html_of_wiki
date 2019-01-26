.PHONY: all
all:
	dune build src/wit/wit.exe src/ohow/ohow.exe

.PHONY: client
client:
	dune build src/client/client.js

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: gen-help
gen-help:
	dune build ohow-help.txt wit-help.txt

.PHONY: clean
clean:
	dune clean
