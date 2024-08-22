# EDIT: THIS VERSION FEATURES 2006 RECTANGLES, A BETTER VIEWING EXPERIENCE,
# AND A LESS HEADACHE_INDUCING GIF

# The album art to "Return to Wherever," one of TWRP's best albums and one of
# my favorite album covers ever. I recommend literally everything on it, but I
# particularly endorse "Generous Dimensions" and "All Night Forever." I made
# this final work using Geometrize, a tool by Sam Twidale to turn an image
# into primitives (in this case OBJECTS rectangles), and then ran it through the
# following AWK script to transform it into MDL script that I could run. Have
# a good summer, everyone.

# The basic gimmick: I'm just taking an image made of two thousand tiny boxes
# all at slightly different z-coordinates and then rotating them around, so
# we can see the side view and some angled views. This compiles significantly
# slower than the old version (which is unsurprising, given that we've doubled
# the number of objects and symbol table lookups (which is O(n) time. oops)
# and tripled the number of frames) -- it runs in about 3 minutes 45 seconds.
# I'm enormously gratified by the speed of this script and my parser, though,
# which complete in a mere .65 seconds combined.

BEGIN {
  WIDTH = 532
  HEIGHT = 532
  OBJECTS = 501
  print "move 250 250 250"
  print "scale " 500 / WIDTH " " 500 / HEIGHT " " 1
  print "ambient 20 20 20"
  print "light 180 180 180 0.5 0 1"
  print "rotate x -90 knobbx"
  print "rotate z -45 knobbz"
  print "vary knobbx easeinout  10  14 0 1"
  print "vary knobbx easeinout  20  24 1 0"
  print "vary knobbx easeinout  40  44 0 1"
  print "vary knobbx easeinout  50  54 1 0"
  print "vary knobbx easeinout  70  74 0 1"
  print "vary knobbx easeinout  80  84 1 0"
  print "vary knobbx easeinout 100 104 0 1"
  print "vary knobbx easeinout 110 114 1 0"
  print "vary knobbz elastic 4 7  15  19 0 1"
  print "vary knobbz elastic 4 7  25  29 1 2"
  print "vary knobbz elastic 4 7  45  49 2 3"
  print "vary knobbz elastic 4 7  55  59 3 4"
  print "vary knobbz elastic 4 7  75  79 4 5"
  print "vary knobbz elastic 4 7  85  89 5 6"
  print "vary knobbz elastic 4 7 105 109 6 7"
  print "vary knobbz elastic 4 7 115 119 7 8"
  print "frames 120"
  print "basename final"
  print ""
  i = 0
}

/"data"/ {
  split($1, data, /[,\[\]]/)
  x = data[2] < data[4] ? data[2] : data[4]
  y = HEIGHT - data[3] < HEIGHT - data[5] ? HEIGHT - data[3] : HEIGHT - data[5]
  draw = "box c" i " " x - WIDTH / 2 " " y - HEIGHT / 2 " " i * 500 / OBJECTS - 250 " " abs (data[4] - data[2]) " " abs (data[3] - data[5]) " " 500 / OBJECTS / 2
}

/"color"/ {
  split($1, colors, /[,\[\]]/)
  colors[2] /= 255 # red
  colors[3] /= 255 # green
  colors[4] /= 255 # blue
  print "constants c" i " " colors[2] / 3 " " colors[2] / 1 " " colors[2] / 1.5 " " colors[3] / 3 " " colors[3] / 1 " " colors[3] / 1.5 " " colors[4] / 3 " " colors[4] / 1 " " colors[4] / 1.5
}

/"score"/ {
  print draw
  i++
}

END {
  print "display"
}
