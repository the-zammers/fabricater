# Fabricater 0.11.0.0
Designed and written by Z Cater

## Overview

These are the docs for *Fabricater*, a command-line image and animation compiler written in Haskell.

Fabricater is run on scripts formatted following [this spec based on our class'](MDL.spec), which specifies the proper usage and purpose of each command. It produces image files or runs ImageMagick to display them directly.

## Installation and usage

Fabricater requires [GHC](https://www.haskell.org/ghc/), [Cabal](https://www.haskell.org/cabal/), and [ImageMagick](https://imagemagick.org/) to run properly. GHC and Cabal come with [GHCup](https://www.haskell.org/ghcup/), which is decidedly *not* necessary but may be easier for your purposes than installing the packages.

To install Fabricater, use `git clone` and navigate to the cloned directory. To run it, run `cabal run fabricater -- [FLAGS]`.

Flags:
- *--script {script} or -f {script}:* path of script file to run; uses stdin otherwise
- *--display {path} or -d {path}:* location to save final image (either static image or animation); uses ImageMagick to display otherwise
- *-D:* do not generate an image; generally used for testing parsing of scripts
- *--output {path} or -o {path}:* location to save parsed script; do not provide parsed data otherwise
- *-O:* print parsed script stdout; do not provide parsed data otherwise

Example 0: I'd like to test the generation of my knob-testing script on my home computer, which I can't use ImageMagick on, so I'm SSHed into Marge and want to save the image to my public_html folder so I can view it on my browser. I'll run `cabal run fabricater -- -f scripts/knobs.txt -d ~/public_html/current.gif`.

Example 1: I've just done a major change to my script and want to see if it still parses. I just want to see the result of the parse, not the actual image, so I'll run `cabal run fabricater -- -f scripts/face.mdl -O -D | less`.

Sample scripts are provided in `scripts/`.
