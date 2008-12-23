.TH EZY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EZY - 
Draws, in a manner determined by the current values of the
control parameters, a complete graph of a single curve
through the points (I,YDRA(I)), for I from 1 to NPTS. The
argument GLAB may be used to specify a "graph label", to be
placed at the top of the graph.
.SH SYNOPSIS
CALL EZY (YDRA,NPTS,GLAB) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ezy (float *ydra, int npts, char *glab)
.SH DESCRIPTION
.IP YDRA 12
(an input array of type REAL, dimensioned at least
NPTS) defines the Y coordinates of points on the curve. The
current value of 'NULL/1.' (default value "1.E36") may be
used in YDRA to signal missing points; curve segments on
either side of a missing point are omitted.
.IP NPTS 12
(an input expression of type INTEGER) is the number of
curve points defined by the array YDRA.
.IP GLAB 12
(an input expression of type CHARACTER) defines a new
"graph label". (If the first character of this expression
is "CHAR(0)", no new "graph label" is defined; the current
one will continue to be used.) A character string defining
a new graph label must either be of the exact length
specified by the current value of 'LINE/MAXIMUM.' (default:
40 characters), or shorter; if shorter, it must be
terminated by the character defined by the current value of
\&'LINE/END.' (default: a '$'). The string becomes the new
text of line number 100 of the label named 'T'.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
agex01,
bnchmk,
example,
tagupw,
tautog,
fagezy.
.SH ACCESS 
To use EZY or c_ezy, load the NCAR Graphics libraries ncarg, ncarg_gks, 
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
ezxy
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
