.TH CONOP1 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CONOP1 - sets option switches for the Conran_family utility.
.SH SYNOPSIS
CALL CONOP1 (IOPT)
.SH DESCRIPTION
.IP IOPT 12
Character, input -- Selects an internal parameter and sets an option
switch.  The possibilities are:
.sp
.nf
\& 'DEF'
\& 'EXT=ON' or 'EXT=OFF'
\& 'GRI=ON' or 'GRI=OFF'
\& 'ITP=C1' or 'ITP=LIN'
\& 'LAB=ON' or 'LAB=OFF'
\& 'LOT=ON' or 'LOT=OFF'
\& 'MES=ON' or 'MES=OFF'
\& 'PDV=ON' or 'PDV=OFF'
\& 'PER=ON' or 'PER=OFF'
\& 'PMM=ON' or 'PMM=OFF'
\& 'PSL=ON' or 'PSL=OFF'
\& 'REP=ON' or 'REP=OFF'
\& 'SCA=ON' or 'SCA=OFF' or 'SCA=PRI'
\& 'TFR=ON' or 'TFR=OFF'
\& 'TOP=ON' or 'TOP=OFF'
\& 'TRI=ON' or 'TRI=OFF'
.fi
.sp
The following options are defined by this subroutine:
.IP DEF 12
This option resets ALL parameters to their default values.
DEF has no 'ON' or 'OFF' states.
.sp
To activate: CALL CONOP1('DEF')
.IP EXT 12
Parameter to set extrapolation.  Normally all
CONRAN versions will only plot the boundaries
of the convex hull defined by the user's data.
To have the contours fill the rectangular
area of the frame, set the EXT switch ON.
The default is OFF.
.sp
To turn on: CALL CONOP1('EXT=ON')
.sp
To turn off: CALL CONOP1('EXT=OFF')
.IP GRI 12
The parameter to display the grid.  GRI is OFF by default.
.sp
To turn on: CALL CONOP1('GRI=ON')
.sp
To turn off: CALL CONOP1('GRI=OFF')
.sp
Note: If GRI=ON, the virtual grid will
be superimposed over the contour plot.
The X and Y tick intervals will be displayed
under the map only if PER=ON.  (see PER)
.IP ITP 12
Set the interpolation scheme.
There are two schemes  --  C1 surfaces and linear.
The C1 method takes longer but will give the
best results when the data is sparse (less
than 100 points).  The linear method will
produce a better plot when there is a dense
data set.  The default is C1 surface.
.sp
For C1 surface CALL CONOP1('ITP=C1')
.sp
For linear  CALL CONOP1('ITP=LIN')
.IP LAB 12
This parameter can be set to either label the contours
(LAB=ON) or not (LAB=OFF).  The default
value is LAB=ON.
(Does not apply to entry CONRAQ.)
.sp
To turn on: CALL CONOP1('LAB=ON')
.sp
To turn off: CALL CONOP1('LAB=OFF')
.IP LOT 12
The parameter to list options on the printer.  The default
value is OFF: no options are displayed.
.sp
To turn on: CALL CONOP1('LOT=ON')
.sp
To turn off: CALL CONOP1('LOT=OFF')
.sp
Note: Print is directed to the standard
output unit.
.IP MES 12
The parameter to plot a message.  The default is ON.
.sp
To turn on: CALL CONOP1('MES=ON')
.sp
To turn off: CALL CONOP1('MES=OFF')
.sp
Note: If MES=ON, a message is printed below
the plot giving contour intervals and execution
time in seconds.  If PER or GRI is ON,
the message also contains the X and Y tick
intervals.
.IP PDV 12
The parameter to plot the input data values.  The
default value is PDV=OFF.
.sp
To turn on: CALL CONOP1('PDV=ON')
.sp
To turn off: CALL CONOP1('PDV=OFF')
.sp
Note: If PDV=ON, the input data values are
plotted relative to their location on the
contour map.  If you only wish to see the
locations and not the values, set PDV=ON and
change FMT to produce an asterisk (*) such as
(I1).
.IP PER 12
The parameter to draw a perimeter.  The default value
is PER=ON, which causes a perimeter to be
drawn around the contour plot.
.sp
To turn on: CALL CONOP1('PER=ON')
.sp
To turn off: CALL CONOP1('PER=OFF')
.sp
Note: If MES is ON, the X and Y tick intervals
will be given.  These are the intervals in user
coordinates that each tick mark represents.
.IP PMM 12
The parameter to plot relative minima and maxima.
(Does not apply to entry CONRAQ.)
This parameter is OFF by default.
.sp
To turn off:  CALL CONOP1('PMM=OFF')
.sp
To turn on:  CALL CONOP1('PMM=ON')
.IP PSL 12
The parameter which sets the plot shield option.
The outline of the shield will be drawn on
the same frame as the contour plot.
(Does not apply to entry CONRAQ.)
By default this option is OFF.
.sp
Draw the shield: CALL CONOP1('PSL=ON')
.sp
Don't draw it: CALL CONOP1('PSL=OFF')
.sp
Note:  See parameter SLD using "man conop3"
to see how the polygon is entered which
defines the shield.
.IP REP 12
The parameter indicating the use of the same data in
a new execution.  The default value is OFF.
.sp
To turn on: CALL CONOP1('REP=ON')
.sp
To turn off: CALL CONOP1('REP=OFF')
.sp
Note: If REP=ON, the same X-Y data and triangulation
are to be used, but it is assumed that
the user has changed the contour values or resolution
for this run.  Scratch arrays WK and IWK must
remain unchanged.
.IP SCA 12
The parameter for scaling of the plot on a frame.
This parameter is ON by default.
.sp
User scaling:  CALL CONOP1('SCA=OFF')
.sp
Program scaling: CALL CONOP1('SCA=ON')
.sp
Prior window:  CALL CONOP1('SCA=PRI')
.sp
If SCA=OFF, plotting instructions
are issued using the user's input coordinates.
SCA=OFF should be used to interface with EZMAP.
If SCA=ON, a viewport is established such that
the plot will fit into the center 90
percent of the frame.  If SCA=PRI, the
program maps the user's plot instructions into
the portion of the frame defined by the
current normalization transformation.
.IP TFR 12
The parameter to advance the frame before triangulation.
The default value is TFR=ON, which means that
the contours and the triangles will be plotted
on separate frames.
.sp
If program set: CALL CONOP1('TFR=ON')
.sp
To turn off:  CALL CONOP1('TFR=OFF')
.sp
Note: Triangles are plotted after the contouring
is completed.  To see the triangles
over the contours, set TFR=OFF.
.IP TOP 12
The parameter to plot only the triangles.
.sp
To turn off: CALL CONOP1('TOP=OFF')
.sp
To turn on: CALL CONOP1('TOP=ON')
.sp
Setting TOP=ON causes parameters TRI and TFR to
be set to TRI=ON, and TFR=OFF.  Setting TOP=OFF
results in TRI=OFF and TFR=ON.  If another setting
is wanted for either TRI or TFR, it should be reset
after the TOP call.
.sp
Note: The user may wish to overlay the triangles
on some other plot.  'TOP=ON' will
allow that.
.IP TRI 12
The parameter to plot the triangulation.  The default is
OFF and therefore the triangles are not drawn.
.sp
To turn on: CALL CONOP1('TRI=ON')
.sp
To turn off: CALL CONOP1('TRI=OFF')
.sp
Note: Plotting the triangles will indicate to
the user where good and bad points of interpolation
are occurring in the contour map.
Equilateral triangles are optimal for interpolation.
Quality degrades as triangles
approach a long and narrow shape.  The convex
hull of the triangulation is also a poor
point of interpolation.
.SH USAGE
CONOP1 is called to toggle Conran_family options before entries
CONRAN, CONRAQ, or CONRAS are called to generate the contour plot.
.SH EXAMPLES
Use the command ncargex to see the following examples: tconaq,
tconan, and tconas.
.SH ACCESS
To use CONOP1 load the NCAR Graphics libraries ncarg, ncarg_gks, and
ncarg_c, preferably in that order.
.SH MESSAGES
See the conran_family man page for a description of all Conran_family error
messages and/or informational messages.
.SH SEE ALSO
Online:
conran_family, conran_family_params, conran, conraq, conras, conop2, conop3,
conop4, conpack, conpack_params
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
