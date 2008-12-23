.TH Bivar_params 3NCARG "November 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Bivar_params - This document briefly describes all Bivar parameters.
.SH DESCRIPTION
Bivar currently supports two parameters. The current values may be retrieved
using one of the routines IDGETI or IDGETR. Parameter values may be reset
using the routine IDSETI or IDSETR.
.sp
The Bivar parameter descriptions appear below in alphabetical
order. Each description begins with a line giving the parameter name 
and the intrinsic FORTRAN type of the parameter.
.IP "\&'ITY' - Integer"
Interpolation Type.  The default value, 0, says that quintic interpolation
should be done.  The value 1 says to use linear interpolation.
.IP "\&'TTY' - Integer"
Triangulation Type.  The default value, 0, says to use a triangulation due
to C. L. Lawson that maximizes the minimum angle occurring in the triangles.
The value 1 says to use the Delaunay triangulation, which ensures triangles
whose circumscribed circles have no data points in their interiors.
.SH SEE ALSO
Online:
bivar, idbvip, idsfft, idpltr, idgeti, idgetr, idseti, idsetr,
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
