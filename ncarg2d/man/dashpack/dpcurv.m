.TH DPCURV 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPCURV - Used to draw a complete curve defined by a sequence of points in the
user coordinate system.
.SH SYNOPSIS
CALL DPCURV (XCPU,YCPU,NPTS)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpcurv (float *xcpu, float *ycpu, int npts);
.SH DESCRIPTION 
.IP XCPU 12
(an input array of type REAL) specifies the X coordinates of points in
the user coordinate system defining a curve to be drawn.
.IP YCPU 12
(an input array of type REAL) specifies the Y coordinates of points in
the user coordinate system defining a curve to be drawn.
.IP NPTS 12
(an input expression of type INTEGER) specifies the number of points
defining the curve.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The FORTRAN statement "CALL DPCURV(XCPU,YCPU,NPTS)" draws a complete curve
by connecting the point (XCPU(1),YCPU(1)) to the point (XCPU(2),YCPU(2)),
connecting that point to the point (XCPU(3),YCPU(3)), and so on, through
the last point, (XCPU(NPTS),YCPU(NPTS)), in a manner described by the
current values of the internal parameters of DASHPACK.
.sp
DPCURV maps the input coordinates to the fractional system and then passes
them on to lower-level routines.  If smoothing is turned off, DPCURV calls
DPDRAW; if smoothing is turned on, it calls DPSMTH.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tdshpk.
.SH ACCESS
To use DPCURV or c_dpcurv, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashpack,
dashpack_params,
dpdraw,
dpfrst,
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
