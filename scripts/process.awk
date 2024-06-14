# The album art to "Return to Wherever," one of TWRP's best albums and one of
# my favorite album covers ever. I recommend literally everything on it, but I
# particularly endorse "Generous Dimensions" and "All Night Forever." I made
# this final work using Geometrize, a tool by Sam Twidale to turn an image
# into primitives (in this case 1000 rectangles), and then ran it through the
# following AWK script to transform it into MDL script that I could run. Have
# a good summer, everyone.
#
# The basic gimmick: I'm just taking an image made of a thousand tiny boxes all
# at slightly different z-coordinates and then rotating them around, so that
# we can see the side view and some angled views. This compiles surprisingly
# fast--a bit less than a second per frame, which is better than I was
# expecting for a thousand objects all covering each other up and all with
# different materials.

BEGIN {
  print "scale " 5 / 6 " " 5 / 6 " " 5 / 6
  print "move 300 300 0"
  print "ambient 20 20 20"
  print "light 180 180 180 0 0 1"
  print "rotate x 90 knobbx"
  print "rotate z -45 knobbz"
  print "vary knobbx easeinout 10 19 0 1"
  print "vary knobbz elastic 4 7 20 24 0 1"
  print "vary knobbx easeinout 25 34 1 0"
  print "vary knobbz elastic 4 7 35 39 1 0"
  print "frames 40"
  print "basename final"
  print ""
  i = 0
}

/"data"/ {
  split($1, data, /[,\[\]]/)
  x = data[2] < data[4] ? data[2] : data[4]
  y = 600 - data[3] < 600 - data[5] ? 600 - data[3] : 600 - data[5]
  draw = "box c" i " " x - 300 " " y - 300 " " i " " abs (data[4] - data[2]) " " abs (data[3] - data[5]) " " 4
}

/"color"/ {
  split($1, colors, /[,\[\]]/)
  colors[2] /= 255 # red
  colors[3] /= 255 # green
  colors[4] /= 255 # blue
  print "constants c" i " " colors[2] / 2 " " colors[2] / 1.5 " " colors[2] " " colors[3] / 2 " " colors[3] / 1.5 " " colors[3] " " colors[4] / 2 " " colors[4] / 1.5 " " colors[4]
}

/"score"/ {
  print draw
  i++
}

END {
  print "display"
}
