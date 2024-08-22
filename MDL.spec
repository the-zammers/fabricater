General Notes

Items seperated by | means you must choose one of them in an input line.
Items enclosed in [] are optional.
Items in capitals are values, not string constants, and should be replaced
    with either a numeric or string value.

For example, rotate is specified as:
rotate x|y|z DEGREES [KNOB]

The following would be valid rotations:
rotate x 20
rotate y 23 k1

While the following would be invalid:
rotate x|y 20
rotate x y 33
rotate x 33 [k1]

Transformations
---------------
All transformations operate as follows:

1. If there is a knob, scale the transformation by the knob value.
2. Alter the top of the coordinate system stack using the transformation.

push    - makes a copy of the top coordinate system and pushes it on top.
pop     - pops off the top coordinate system.

move  X Y Z [KNOB]
scale X Y Z [KNOB]
rotate x|y|z DEGREES [KNOB]
        - only one axis may be specified at a time

Image creation
--------------
All image creation commands operate as follows:

1. Generate all the points and edges for the object in question.
2. Transform the points against the top of the coordinate system stack.
3. Render the object.

sphere [COLOR] X Y Z R
        - defaults to grey
torus  [COLOR] X Y Z R0 R1
        - R0 = cross-section radius
        - R1 = torus radius
        - defaults to grey
box    [COLOR] X0 Y0 Z0 H W D
        - (X0,Y0,Z0) = one corner of the box
        - H W D = height width and depth
        - defaults to grey

line X0 Y0 Z0 X1 Y1 Z1
circle X Y Z R
qbezier X0 Y1 X1 Y1 X2 Y2
        - (X0,Y0) and (X2,Y2) = endpoints
        - (X1,Y1)              = control point
bezier X0 Y0 X1 Y1 X2 Y2 X3 Y3
        - (X0,Y0) and (X3,Y3) = endpoints
        - (X1,Y1) and (X2,Y2) = control points
hermite X0 Y0 X1 Y1 X2 Y2 X3 Y3
        - (X0,Y0) and (X1,Y1) = endpoints
        - (X2,Y2) and (X3,Y3) = rates of change at endpoints

Knobs/Animation
---------------
basename NAME
        - sets the base filename to save under if an animation.
        - e.g. `basename zammers` saves zammers_001.png, zammers_002.png, etc
        - defaults to img but generates a warning if needed and not provided
frames NUM_FRAMES
        - how many frames to generate in total.
        - defaults to 1
set KNOB VALUE
        - sets a knob's value.
vary KNOB [constant|linear|easein|easeout|easeinout
          |elastic DAMPING FREQUENCY|bounce DAMPING FREQUENCY]
          START_FRAME END_FRAME START_VAL END_VAL
        - vary a knob over the course of the given frames.
        - constant is a constant change, remaining at the starting value
          forever regardless of whether it's in the given frame range
        - linear is a linear change
        - easein is an exponential easing-in function
        - easeout is an exponential easing-out function
        - easeinout is a cubic function that eases both in AND out
        - elastic is an elastic function (exponential and sinusoidal) that
          OVERSHOOTS and takes two additional arguments
        - bounce is a bounce function (exponential and sinusoidal) that
          WILL NEVER GO PAST THE FINAL VALUE and takes two arguments
        - All of these functions are GUARANTEED to start and end at the given
          values on the given frames, except for constant, elastic, and bounce,
          which may be slightly off, depending on the user-provided values.
        - defaults to linear
        - e.g. `vary knobby bounce 4 3 10 19 5 1` will vary the knob "knobby"
          from frames 10 through 19 from 5 to 1, bouncing slightly towards the
          end with a dampening of 4 and a frequency of 3 (arbitrary values)

Lighting
--------
light R G B X Y Z [_|KNOBX _|KNOBY _|KNOBZ]
        - creates a pointlight with rgb values R,G,B at location X,Y,Z.
        - overrides X, Y, and/or Z with values of KNOBX, KNOBY, or KNOBZ
        - e.g. `light 100 100 100 0 0 1 cat _ _` makes a pointlight at (0,0,1)
          of color #646464 whose x-coordinate changes with the value of "cat".
ambient r g b
        - creates an ambient light with rgb values R,G,B.
        - only one can exist at a time
constants NAME AR DR SR AG DG SG AB DB SB
        - saves a set of lighting components as NAME.
        - a = ambient, d = diffuse, and s = specular reflections

MISC
----
//      - comment to the end of a line.
save FILENAME
        - save the image in its current state under the name FILENAME.
display - display the current image on the screen.
clear   - clears the current image (generally not necessary).
