.TH LINE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
LINE - draws a line from the point (X1,Y1) to the point (X2,Y2).
The "pen" (for subsequent calls to FRSTPT, VECTOR, PLOTIF, and PLOTIT)
is left at (X2,Y2).
.SH SYNOPSIS
CALL LINE (X1,Y1,X2,Y2)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_line (float x1, float y1, float x2, float y2)
.SH DESCRIPTION 
.IP X1 12
(an input expression of type REAL) defines the X user coordinate of
the starting point of a line segment.
.IP Y1 12
(an input expression of type REAL) defines the Y user coordinate of
the starting point of a line segment.
.IP X2 12
(an input expression of type REAL) defines the X user coordinate of
the ending point of a line segment.
.IP Y2 12
(an input expression of type REAL) defines the Y user coordinate of
the ending point of a line segment.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Polyline type, line width, and color can be set by calling the
GKS routines GSLN, GSLWSC, GSCR, and GSPLCI before calling LINE.
.sp
This routine flushes the SPPS polyline buffer at each line draw.  Thus,
polyline attributes can be safely changed between successive calls.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
mpex07, mpex09, cbex01, coex01, coex03, epltch, srex01, and tgflas.
.SH ACCESS
To use LINE, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_line, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gpl, gsln, gslwsc, gscr, gsplci,
spps, spps_params, frstpt, vector, plotif, curve, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
