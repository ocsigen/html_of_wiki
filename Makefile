.PHONY: all
all:
	dune build @all

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
