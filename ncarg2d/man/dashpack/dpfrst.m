.TH DPFRST 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPFRST - Specifies the first point in a sequence of points, in the user
coordinate system, defining a curve to be drawn.
.SH SYNOPSIS
CALL DPFRST (XCPU,YCPU)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpfrst (float xcpu, float ycpu);
.SH DESCRIPTION 
.IP XCPU 12
(an input expression of type REAL) specifies the X coordinate of a point,
in the user coordinate system.
.IP YCPU 12
(an input expression of type REAL) specifies the Y coordinate of a point,
in the user coordinate system.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
Call DPFRST to do a "pen-up" move to the first of a sequence of points
defining a curve.  Call DPVECT to do "pen-down" moves to each of the other
points in the sequence and then call DPLAST to finish drawing the curve
and flush the buffers.
.sp
DPFRST maps the input coordinates to the fractional coordinate system and
then does a "pen-up" call to either DPDRAW (if smoothing is turned off)
or to DPSMTH (if smoothing is turned on) with those coordinates.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tdshpk.
.SH ACCESS
To use DPFRST or c_dpfrst, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashpack,
dashpack_params,
dpcurv,
dpdraw,
dpgetc,
dpgeti,
dpgetr,
dplast,
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
