.PHONY: default run test clean

default: run

run:
	cabal run fabricater -- --script scripts/knobs.mdl

test:
	@cabal run -v0 fabricater -- -O -D -f scripts/knobs.mdl

clean:
	rm -rf dist-newstyle
	rm -f *.ppm
	rm -f *.png
	rm -f formatted.txt
