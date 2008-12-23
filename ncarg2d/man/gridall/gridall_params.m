.TH Gridall_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Gridall_params - This document briefly describes all Gridall
internal parameters.
.SH DESCRIPTION 
GRIDAL has twenty-two internal parameters, each of which
affects, in some way, the behavior of the routines that
draw backgrounds. The current value of a parameter may be
retrieved by calling GAGETC, GAGETI, and/or GAGETR and it
may be reset by calling GASETC, GASETI, and/or GASETR;
certain parameters may also be reset by calling other,
older, routines (GACOLR, LABMOD, TICKS, and TICK4). 
.sp
Parameter descriptions, in alphabetical order, of all Gridall
internal parameters follow. Each description begins with a line
giving the parameter name and the intrinsic FORTRAN type of the
parameter.
.IP "\'CAX\' - Integer"
The polyline color index to be used for
drawing the axes. A negative value
implies that no change is to be made in
the polyline color index before drawing
the axes. The default value of \'CAX\' is
-1; its value may be changed by calling
GASETI, GASETR, or GACOLR (argument
KAXS) .
.IP "\'CLB\' - Integer"
The polyline/text color index to be
used for drawing labels. (Both are
affected because the labels may be
drawn using calls to WTSTR or calls to
PLCHHQ.) A negative value implies that
no change is to be made in the polyline
and text color indices before drawing
the labels. The default value of \'CLB\'
is -1; its value may be changed by
calling GASETI, GASETR, or GACOLR
(argument KLBL).
.IP "\'CMJ\' - Integer" 
The polyline color index to be used for
major ticks and/or grid lines. A
negative value implies that no change
is to be made in the polyline color
index before drawing major ticks/grid
lines. The default value of \'CMJ\' is -1;
its value may be changed by calling
GASETI, GASETR, or GACOLR (argument
KMJT).
.IP "\'CMN\' - Integer"
The polyline color index to be used for
minor ticks and/or grid lines. A
negative value implies that no change
is to be made in the polyline color
index before drawing minor ticks/grid
lines. The default value of \'CMN\' is -1;
its value may be changed by calling
GASETI, GASETR, or GACOLR (argument
KMNT).
.IP "\'LTY\' - Integer"
The labeling type. The value 0 implies
that the SPPS routine WTSTR is to be
called to draw labels, the value 1 that
the Plotchar routine PLCHHQ is to be
called. The default value of \'LTY\' is
0; its value may be changed by calling
GASETI or GASETR.
.IP "\'WAX\' - Real"
The line width to be used for drawing
the axes. A value less than or equal to
zero implies that no change is to be
made in the line width scale factor
before drawing the axes. The default
value of \'WAX\' is 0; its value may be
changed by calling GASETI or GASETR.
.IP "\'WLB\' - Real"
The line width to be used for drawing
labels. (Labels drawn using calls to
the Plotchar routine PLCHHQ are
affected by this; those drawn using
calls to the SPPS routine WTSTR are
not.) A value less than or equal to
zero implies that no change is to be
made in the line width scale factor
before drawing the labels. The default
value of \'WLB\' is 0; its value may be
changed by calling GASETI or GASETR.
.IP "\'WMJ\' - Real"
The line width to be used for major
ticks and/or grid lines. A value less
than or equal to zero implies that no
change is to be made in the line width
scale factor before drawing. The
default value of \'WMJ\' is 0; its value
may be changed by calling GASETI or
GASETR.
.IP "\'WMN\' - Real"
The line width to be used for minor
ticks and/or grid lines. A value less
than or equal to zero implies that no
change is to be made in the line width
scale factor before drawing. The
default value of \'WMN\' is 0; its value
may be changed by calling GASETI or
GASETR.
.IP "\'XLF\' - Character"
The format to be used for labels on the
X axis. The character string must begin
with a left parenthesis and end with a
right parenthesis and it must not
exceed ten characters in length.
Conversions of types E, F, G, and I are
allowed. The default value of \'XLF\' is
\'(E10.3)\'; its value may be changed by
calling GASETC or LABMOD (argument
FMTX).
.IP "\'XLL\' - Integer"
The length of each X-axis label. If
\'XLL\' is given a non-zero value "n" and
LBLX is a string produced by the format
\'XLF\', then the label will be the
substring LBLX(1:n). If, on the other
hand, \'XLL\' = 0, then the label will be
the substring LBLX(m:n), where
LBLX(m:m) is the first non-blank
character in LBLX and LBLX(n:n) is the
last non-blank character following
LBLX(m:m). Using a non-zero value for
\'XLL\' causes labels to be centered
differently than if a zero value is
used. The default value of \'XLL\' is 0;
its value may be changed by calling
GASETI, GASETR, or LABMOD (argument
NUMX).
.IP "\'XLO\' - Real"
The (vertical) distance of an X-axis
label from the X axis. Values between 0
and 1 are interpreted as fractions of
the width of the plotter frame, while
values greater than 1 are interpreted
as plotter address units (PAUs). The
value is interpreted as the distance
from the bottom edge of the viewport to
the nearest Y address of an X-axis
label; negative values may be used to
put labels above the viewport. The
value 0 is treated as equivalent to 20
and the value 1 is treated as
equivalent to -20-h, where h is the
height of the viewport in plotter
address units. The default value of
\'XLO\' is 20, which means 20 PAUs; its
value may be changed by calling GASETI,
GASETR, or LABMOD (argument IYDC).
(Note that the LABMOD argument IXDC
corresponds to \'YLO\' and that the
LABMOD argument IYDC corresponds to
\'XLO\', which is perhaps somewhat
counterintuitive.)
.IP "\'XLS\' - Real"
The size (width) of characters in X-axis
labels. Values between 0 and 1 are
interpreted as fractions of the width
of the plotter frame, while values
greater than or equal to 1 are
interpreted as plotter address units
(PAUs). The integral values 0., 1., 2.,
and 3. mean 8, 12, 16, and 24 PAUs,
respectively. Values less than zero are
treated as zero. The default value of
\'XLS\' is 10, which means 10 PAUs; its
value may be changed by calling GASETI,
GASETR, or LABMOD (argument ISZX).
.IP "\'XMJ\' - Real"
The length of major ticks on the X
axis.  ABS(\'XMJ\') specifies the length
of major ticks on the X axis; values
between 0 and 1 are interpreted as
fractions of the width of the plotter
frame, while values greater than or
equal to 1 are interpreted as plotter
address units (PAUs). If \'XMJ\' is
positive, the ticks point inward; if
\'XMJ\' is negative, the ticks point
outward. The default value of \'XMJ\' is
12, which means 12 PAUs; its value may
be changed by calling GASETI, GASETR,
or TICKS (argument LMJX).
.IP "\'XMN\' - Real"
The length of minor ticks on the X
axis. ABS(\'XMN\') specifies the length
of minor ticks on the X axis; values
between 0 and 1 are interpreted as
fractions of the width of the plotter
frame, while values greater than or
equal to 1 are interpreted as plotter
address units (PAUs). If \'XMN\' is
positive, the ticks point inward; if
\'XMN\' is negative, the ticks point
outward. The default value of \'XMN\' is
8, which means 8 PAUs; its value may be
changed by calling GASETI, GASETR, or
TICKS (argument LMNX).
.IP "\'XOR\' - Integer"
The orientation of X-axis labels. The
value 0 implies the use of horizontal
labels, while the value 1 implies the
use of vertical labels. The default
value is 0; its value may be changed by
calling GASETI, GASETR, or LABMOD
(argument IXOR).
.IP "\'YLF\' - Character"
The format to be used for labels on the
Y axis. The character string must begin
with a left parenthesis and end with a
right parenthesis and it must not
exceed ten characters in length.
Conversions of types E, F, G, and I are
allowed. The default value of \'YLF\' is
\'(E10.3)\'; its value may be changed by
calling GASETC or LABMOD (argument
FMTY).
.IP "\'YLL\' - Integer"
The length of each Y-axis label. If
\'YLL\' is given a non-zero value "n" and
LBLY is a string produced by the format
\'YLF\', then the label will be the
substring LBLY(1:n). If, on the other
hand, \'YLL\' = 0, then the label will be
the substring LBLY(m:n), where
LBLY(m:m) is the first non-blank
character in LBLY and LBLY(n:n) is the
last non-blank character following
LBLY(m:m). Using a non-zero value for
\'YLL\' causes labels to be centered
differently than if a zero value is
used. The default value of \'YLL\'is 0;
its value may be changed by calling
GASETI, GASETR, or LABMOD (argument
NUMY).
.IP "\'YLO\' - Real"
The (horizontal) offset distance of a Y-axis
label from the Y axis. Values
between 0 and 1 are interpreted as
fractions of the width of the plotter
frame, while values greater than 1 are
interpreted as plotter address units
(PAUs). The value is interpreted as the
distance from the left edge of the
viewport to the nearest X address of a
Y-axis label; negative values may be
used to put labels to the right of the
viewport. The value 0 is treated as
equivalent to 20 and the value 1 is
treated as equivalent to -20-w, where w
is the width of the viewport in plotter
address units. The default value of
\'YLO\' is 20, which means 20 PAUs; its
value may be changed by calling GASETI,
GASETR, or LABMOD (argument IXDC).
(Note that the LABMOD argument IXDC
corresponds to \'YLO\' and that the
LABMOD argument IYDC corresponds to
\'XLO\', which is perhaps somewhat
counterintuitive.)
.IP "\'YLS\' - Real"
The size (width) of characters in Y-axis
labels. Values between 0 and 1 are
interpreted as fractions of the width
of the plotter frame, while values
greater than or equal to 1 are
interpreted as plotter address units
(PAUs). The integral values 0., 1., 2.,
and 3. mean 8, 12, 16, and 24 PAUs,
respectively. Values less than zero are
treated as zero. The default value of
\'YLS\' is 10, which means 10 PAUs; its
value may be changed by calling GASETI,
GASETR, or LABMOD (argument ISZY).
.IP "\'YMJ\' - Real"
The length of major ticks on the Y axis.
ABS(\'YMJ\') specifies the length of
major ticks on the Y axis; values
between 0 and 1 are interpreted as
fractions of the width of the plotter
frame, while values greater than or
equal to 1 are interpreted as plotter
address units (PAUs). If \'YMJ\' is
positive, the ticks point inward; if
\'YMJ\' is negative, the ticks point
outward. The default value of \'YMJ\' is
12, which means 12 PAUs; its value may
be changed by calling GASETI, GASETR,
or TICKS (argument LMJY).
.IP "\'YMN\' - Real"
The length of minor ticks on the Y
axis. ABS(\'YMN\') specifies the length
of minor ticks on the Y axis; values
between 0 and 1 are interpreted as
fractions of the width of the plotter
frame, while values greater than or
equal to 1 are interpreted as plotter
address units (PAUs). If \'YMN\' is
positive, the ticks point inward; if
\'YMN\' is negative, the ticks point
outward. The default value of \'YMN\' is
8, which means 8 PAUs; its value may be
changed by calling GASETI, GASETR, or
TICKS (argument LMNY).
.SH SEE ALSO
Online:
gridall,
gacolr,
gagetc,
gageti,
gagetr,
gasetc,
gaseti,
gasetr,
labmod,
tick4,
ticks.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
