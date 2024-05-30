.PHONY: default run view format clean

default: run

run:
	cabal run fabricater -- --script scripts/face.mdl

view:
	cabal run fabricater -- --script scripts/jinkougaku.mdl --display $(HOME)/public_html/current.png

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
