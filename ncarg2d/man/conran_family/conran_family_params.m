.TH Conran_family_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Conran_family_params - Includes a brief description of all Conran_family
internal parameters.
.SH DESCRIPTION 
There are three Conran entries, CONRAN, CONRAQ, and CONRAS.  CONRAN
can be invoked in two ways.  One generates smoothed contours, the
other does not.  CONRAQ is a quick version of the package which has
a more limited set of options than the other entries.  CONRAS is
the super version of the package.
.sp
Parameters are set using the CONOP1, CONOP2, CONOP3, and CONOP4
entries.  CONOP1 sets option switches, CONOP2 sets option switches
and option related integer arguments, CONOP3 sets option switches
and option related real arguments, and
CONOP4 sets option switches and option related character arguments.
This man page provides a functional overview of the parameters.
See the man pages for the CONOPx entries for precise information
on how to make the option setting calls.
.sp
All of the following parameters apply to
normal, smooth, or super versions of the package.
The inclusive set of Conran parameters and their switch defaults:
.nh

CONOP1 parameters  (Switches)   [16]

   DEF (all options begin with defaults)
   EXT=OFF   MES=ON    REP=OFF
   GRI=OFF   PDV=OFF   SCA=ON
   ITP=C1    PER=ON    TFR=ON
   LAB=ON    PMM=OFF   TOP=OFF
   LOT=OFF   PSL=OFF   TRI=OFF

CONOP2 parameters  (Switches & Integer args)   [7]

   INT=OFF LSZ=OFF NCP=OFF SML=OFF
   SPD=OFF SSZ=OFF STL=OFF

CONOP3 parameters  (Switches & Real args)      [7]

   CHL=OFF CIL=OFF CON=OFF DBP=OFF
   SDC=OFF SLD=OFF TEN=OFF

CONOP4 parameters  (Switches & Character args)  [3]

   DAS=OFF FMT=OFF TLE=OFF

.fi
Parameters which are not available in CONRAQ, the
quick version of the Conran_family:
.sp
DAS, DBP, LAB, LSZ, PMM, PSL, SDC, SLD, SML, TEN
.SH
Parameters which set option switches in calls to entry CONOP1:
.IP DEF 12
This parameter provides a quick way to reset
all parameters to their default values.
.IP EXT 12
A parameter to set contour extrapolations.
The default is to only plot within the boundaries
of the convex hull defined by the user's data.
To have the contours fill the rectangular
area of the frame, set the EXT switch ON.
.IP GRI 12
The parameter to display the grid.  GRI is OFF by default.
Note: If GRI=ON, the virtual grid will
be superimposed over the contour plot.
The X and Y tick intervals will be displayed
under the map only if PER=ON.  (See parameter PER)
.IP ITP 12
Set the interpolation scheme.
There are two schemes  --  C1 surfaces and linear.
The C1 method (ITP=C1) takes longer but will give the
best results when the data is sparse (less
than 100 points).  The linear method (ITP=LIN) will
produce a better plot when there is a dense
data set.  The default is a C1 surface.
.IP LAB 12
This parameter determines whether to label the contours
(LAB=ON) or not (LAB=OFF).  The default
value is to label the contours.
(Does not apply to entry CONRAQ.)
.IP LOT 12
The parameter to list options on the printer.  The default
value is to not display the options.
.IP MES 12
The parameter to plot a message.  The default is
that a message is printed below
the plot giving contour intervals and execution
time in seconds.  If PER or GRI is ON,
the message also contains the X and Y tick
intervals.
.IP PDV 12
The parameter to plot the input data values.  The
default value is to not plot the input data.
.IP PER 12
The parameter to draw a perimeter.  The default
is to draw a perimeter
around the contour plot.
.IP PMM 12
The parameter to plot relative minima and maxima.
The default is to not plot them.
(Does not apply to entry CONRAQ.)
.IP PSL 12
The parameter which determines whether the
outline of the data shield will be drawn on
the same frame as the contour plot.
The default is to not draw the shield.
(Does not apply to entry CONRAQ.)
.IP REP 12
The parameter indicating the use of the same data in
a new execution.  The default value is to assume a
new input dataset.
.IP SCA 12
The parameter for scaling of the plot on a frame.
This parameter is ON by default.
.IP TFR 12
The parameter to advance the frame before triangulation.
The default value is TFR=ON, which means that
the contours and the triangles will be plotted
on separate frames.
.IP TOP 12
The parameter to plot only the triangles.
The default is to plot contours, then triangles.
.IP TRI 12
The parameter to plot the triangulation.  The default is
OFF and therefore the triangles are not drawn.
.SH
Parameters of type INTEGER set in calls to entry CONOP2:
.IP INT 12
The parameter to determine line intensities for
various parts of the plot.  Intensities vary
from a high of 255 to a low of 0.  The default
is high for all parts
of the plot except minor contour lines which
are set to low.
.IP LSZ 12
This parameter determines the character size of
contour labels.
The default value is 9, which results in
label characters of
a size of 9./1023. NDC units.
(Does not apply to entry CONRAQ.)
.IP NCP 12
The parameter NCP controls the
number of data points to be used in the
interpolation.  Increasing NCP causes more
of the  surrounding data to influence the
point of interpolation.  In the case of linear interpolation
NCP is always 4.  In the case of C1 interpolation, NCP
can vary from 2 to 25 with 4 as the default.
.sp
The interpolation option is selected using internal parameter ITP.
.IP SML 12
This parameter specifies the character size of minimum and
maximum contour labels.
The default value is 15, which results in
label character sizes of 15./1023. NDC units.
(Does not apply to entry CONRAQ.)
.IP SPD 12
The parameter for the size of the plotted input data values.
The default value is 8, which results in
a data value character size of 8./1023. NDC units.
.IP SSZ 12
The parameter to determine the resolution (number of
steps in each direction).  The default is 40.
.IP STL 12
The parameter to determine the size of the main title characters.
The default value is 16, which results in title character
sizes of 16./1023. NDC units.
.SH
Parameters of type REAL set in calls to entry CONOP3:
.IP CHL 12
This parameter determines how the high and low
contour values are set.  The default is to compute
the high and low values from the input data.
.IP CIL 12
This parameter determines how the contour increment
is set.  The default is to calculate the increment
based on the range of the input data.
.IP CON 12
This parameter determines how the contour levels
are set.  The default is to have the program
compute the contour values and the number of
contour levels.
.IP DBP 12
This parameter determines how the dash pattern
break point (BP) is set.  The default is BP=0.
(Does not apply to entry CONRAQ.)
.IP SDC 12
The parameter to determine how to scale the data on
the contours.
The default scaling is 1.
(Does not apply to entry CONRAQ.)
.IP SLD 12
This parameter allows for the entry of a
bounding polygon (shield).  Only contours within
the shield are drawn.  The default is no shield.
(Does not apply to entry CONRAQ.)
.IP TEN 12
The parameter to determine the tension factor applied
when smoothing contour lines.
The default value is 2.5.
(Does not apply to entry CONRAQ.)
.SH
Parameters of type CHARACTER set in calls to entry CONOP4:
.IP DAS 12
This parameter determines which contours are
represented by dashed lines.  The default is
to draw all contours as solid lines.
(Does not apply to entry CONRAQ.)
.IP FMT 12
A parameter which specifies a format for output of
the data values.  If FMT=OFF, the default values of
the format, the number of characters in the format,
and the length of the output field described in the
format, are (G10.3), 7, and 10, respectively.
.IP TLE 12
The parameter to place a title at the top of the plot.
The default value is no title.
.SH SEE ALSO
Online:
conran_family, conran, conraq, conras, conop1, conop2, conop3, conop4,
conpack, conpack_params
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
