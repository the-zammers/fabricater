[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/mHYDusTj)

# Fabricater 0.11.0.0
Designed and written by Z Cater

## Overview

These are the docs for *Fabricater*, a command-line MDL compiler written in Haskell.

The goals of my final project are:

### To ensure that my compiler is MDL-compliant

### To implement lights as symbols on the symbol table (MDL compliant)
- `light r g b x y z` creates a "light" datastructure with rgb values r,g,b at location x,y,z.
- `ambient r g b` specifies how much ambient light is in the scene.

### To implement knob-based animations (MDL compliant)
- `basename name` sets the base filename to save under.
- `set knobname value` sets a knobs value in the symbol table.
- `frames num_frames` declares how many frames to generate all together.
- `vary knob start_frame end_frame start_val end_val` varies a knob from start_val to end_val over the course of start_frame to end_frame.
*NOTE: DID NOT IMPLEMENT TWEEN, SAVEKNOBS, OR SETKNOBS* -- couldn't find a clean way to modify the knobs while the program was running rather than during the single parsing pass

### To enhance the power of animation knobs beyond mere linear changes (NOT MDL compliant)
Follow the knobname in `vary` with an optional string declaring the type of change:
- `constant` is a constant change, remaining at the starting value forever regardless of whether it's between the start and end frames.
- `linear` is a linear change, the default variety if no string is provided.
- `easein` is an exponential easing-in function.
- `easeout` is an exponential easing-out function.
- `easeinout` is a cubic function that both eases in AND out.
- `elastic` is an elastic function (exponential and sinusoidal) and OVERSHOOTS -- this takes two arguments. The first is dampening (higher values means greater dampening) and the second is frequency (higher values means higher frequency).
- `bounce` is a bounce function (exponential and sinusoidal) and WILL NEVER GO PAST THE FINAL VALUE -- this takes two arguments. The first is dampening (higher values means greater dampening) and the second is frequency (higher values means higher frequency).
All of these functions are GUARANTEED to start and end at the given values on the given frames, except for the last two, which may be slightly off, depending on the user-provided values.

### To allow animation knobs to modify lighting colors and pointlight positions (NOT MDL compliant)
- Following the six arguments to the MDL `light` function can one optionally include exactly three knob names overriding the x, y, and z values respectively. Any of these can be replaced with an underscore to do nothing and leave the original values as they were.

## How to run Fabricater:

These docs really should've been written down a while ago, but they weren't. Oops. To run Fabricater, run `cabal run fabricater -- [FLAGS]`.

Flags:
- *--script {script} or -f {script}:* declares script file to run; uses stdin otherwise
- *--display {path} or -d {path}:* declares location to save final image to (either static image or animation); uses imagemagick to display otherwise
- *-D:* declares that the image will not be generated; generally used for testing parsing of scripts
- *--output {path} or -o {path}:* declares location to save parsed script to (used for debugging and as a verbose mode); does not provide parsed data otherwise
- *-O:* declares that the parsed script should be printed to stdout; generally used for testing parsing of short scripts

Example 1: I'd like to test the generation of my knob-testing script on my home computer, which I can't use ImageMagick on, so I'm SSHed into Marge and want to save the image to my public_html folder so I can view it on my browser. I'll run `cabal run fabricater -- -f scripts/knobs.txt -d ~/public_html/current.gif`.

Example 2: I've just done a major change to my parsing and want to see if it still works. I just want to see the result of the parse, not the actual image, so I'll run `cabal run fabricater -- -f scripts/face.mdl -O -D | less`.
