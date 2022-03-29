.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	dune exec CreatureGame

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f adventure.zip
	zip -r adventure.zip . -x@exclude.lst

clean:
	dune clean
	rm -f adventure.zip

doc:
	dune build @doc