.TH VECTOR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VECTOR - generates a pen-down move to the point (PX,PY) in the user
coordinate system.
It is used in conjunction with routine FRSTPT to draw lines.
.SH SYNOPSIS
CALL VECTOR (PX,PY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vector (float px, float py)
.SH DESCRIPTION 
.IP PX 12
(an input coordinate of type REAL) defining the X user coordinate.
.IP PY 12
(an input coordinate of type REAL) defining the Y user coordinate.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Polyline type, line width, and color can be set by calling the
GKS routines, GSLN, GSLWSC, GSCR, and GSPLCI, before calling VECTOR.
.sp
Note that this routine buffers points for efficiency.  Thus one must
be sure to flush the buffer before changing polyline attributes.
CALL PLOTIF (0.,0.,2) will flush the buffer.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
tgflas.
.SH ACCESS
To use VECTOR, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_vector, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gpl, gsln, gslwsc, gscr, gsplci,
spps, spps_params, frstpt, line, curve, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
