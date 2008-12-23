.TH FRSTPT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FRSTPT - generates a "pen-up" move to a specified point in the user
coordinate system.  FRSTPT is used in conjunction with the routine VECTOR
to draw lines.
.SH SYNOPSIS
CALL FRSTPT (PX,PY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_frstpt (float px, float py)
.SH DESCRIPTION 
.IP PX 12
(an input expression of type REAL) defines the X user coordinate.
.IP PY 12
(an input expression of type REAL) defines the Y user coordinate.
.SH C-BINDING DESCRIPTION                               ,,
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Polyline type, line width, and color can be set by calling the
GKS routines GSLN, GSLWSC, GSCR, and GSPLCI before calling FRSTPT.
.sp
Note that, for the sake of efficiency, the routines FRSTPT and VECTOR
buffer the polylines resulting from pen moves.  One must be sure to flush
the SPPS polyline buffer before changing polyline attributes.
A "CALL PLOTIF (0.,0.,2)" will flush the buffer.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tgflas.
.SH ACCESS
To use FRSTPT or c_frstpt, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gpl, gsln, gslwsc, gscr, gsplci,
spps, spps_params, vector, line, curve, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
