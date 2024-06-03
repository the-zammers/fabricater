.PHONY: default run view test format clean

default: run

run:
	cabal run fabricater -- --script scripts/knobs.txt

view:
	cabal run fabricater -- --script scripts/face.mdl --display $(HOME)/public_html/current.png

test:
	@cabal run -v0 fabricater -- -O -D -f scripts/knobs.txt

format:
	@cabal run -v0 fabricater -- -O -D

docs:
	cabal haddock --haddock-all
	rm -rf ~/public_html/fabricater
	mv dist-newstyle/build/x86_64-linux/ghc-9.4.8/fabricater-0.6.0.0/x/fabricater/doc/html/fabricater/fabricater/ ~/public_html

clean:
	rm -rf dist-newstyle
	rm -f *.ppm
	rm -f *.png
	rm -f formatted.txt
