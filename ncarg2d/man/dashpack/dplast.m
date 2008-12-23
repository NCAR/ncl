.TH DPLAST 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPLAST - Says that the preceding call to DPVECT defined the final point in
a sequence of points, in the user coordinate system, defining a curve to be
drawn.
.SH SYNOPSIS
CALL DPLAST
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dplast ();
.SH DESCRIPTION 
There are no arguments.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
Call DPFRST to do a "pen-up" move to the first of a sequence of points
defining a curve.  Call DPVECT to do "pen-down" moves to each of the other
points in the sequence and then call DPLAST to finish drawing the curve
and flush the buffers.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tdshpk.
.SH ACCESS
To use DPLAST or c_dplast, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashpack,
dashpack_params,
dpcurv,
dpdraw,
dpfrst,
dpgetc,
dpgeti,
dpgetr,
dpline,
dpsetc,
dpseti,
dpsetr,
dpsmth,
dpvect,
ncarg_cbind.
.sp
Hardcopy:
None.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
