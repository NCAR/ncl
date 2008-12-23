.TH EZXY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EZXY - 
Draws, in a manner determined by the current values of the
control parameters, a complete graph of a single curve
through the points (XDRA(I),YDRA(I)), for I from 1 to NPTS.
The argument GLAB may be used to specify a "graph label",
to be placed at the top of the graph.
.SH SYNOPSIS
CALL EZXY (XDRA,YDRA,NPTS,GLAB) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ezxy (float *xdra, float *ydra, int npts, char *glab)
.SH DESCRIPTION
.IP XDRA 12
(an input array of type REAL, dimensioned NPTS)
defines the X coordinates of points on the curve.
.IP YDRA 12
(an input array of type REAL, dimensioned NPTS)
defines the Y coordinates of points on the curve.
.sp
The points on the curve have coordinates (XDRA(I),YDRA(I)),
for I from 1 to NPTS. The current value of 'NULL/1.'
(default value "1.E36") may be used to signal missing data
in these arrays. If either coordinate of a point is
missing, the point is considered to be missing; curve
segments on either side of a missing point are not drawn.
Note:  Because all non-missing coordinates are used in
figuring the minimum and maximum user values along a given
axis, it is safest to mark both coordinates as "missing".
.IP NPTS 12
(an input expression of type INTEGER) is the number of
curve points defined by the arrays XDRA and YDRA.
.IP GLAB 12
(an input expression of type CHARACTER) defines a new
"graph label". (If the first character of this expression
is "CHAR(0)", no new "graph label" is defined; the current
one will continue to be used.)  A character string defining
a new graph label must either be of the exact length
specified by the current value of 'LINE/MAXIMUM.' (default:
40 characters), or shorter; if shorter, it must be
terminated by the character defined by the current value of
'LINE/END.' (default: a '$'). The string becomes the new
text of line number 100 of the label named 'T'.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
agex02,
agex06,
agex09,
agex10,
agex11,
agex12,
splogy,
sprevx,
tagupw,
tautog,
fagezxy,
fspponts.
.SH ACCESS 
To use EZXY or c_ezxy, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.    To get smoother curves, 
drawn using spline interpolation, also load libdashsmth.o.  Or,
you can use the ncargf77 command to compile your program and load 
the above libraries, then, to get smoother curves, use the 
-dashsmth option.
.SH SEE ALSO
Online:
autograph,
agback,
agbnch,
agchax,
agchcu,
agchil,
agchnl,
agcurv,
agdshn,
aggetc,
aggetf,
aggeti,
aggetp,
aggetr,
agpwrt,
agrstr,
agsave,
agsetc,
agsetf,
agseti,
agsetp,
agsetr,
agstup,
agutol,
anotat,
displa,
ezmxy,
ezmy,
ezy
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
