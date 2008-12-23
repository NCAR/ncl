.TH DPDRAW 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPDRAW - Used to draw a curve when fractional coordinates are available and
no smoothing is to be done.
.SH SYNOPSIS
CALL DPDRAW (XCPF,YCPF,IFVL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpdraw (float xcpf, float ycpf, int ifvl);
.SH DESCRIPTION 
.IP XCPF 12
(an input expression of type REAL) specifies the X coordinate of a point in
the fractional coordinate system.  Ignored when IFVL = 2.
.IP YCPF 12
(an input expression of type REAL) specifies the Y coordinate of a point in
the fractional coordinate system.  Ignored when IFVL = 2.
.IP IFVL 12
(an input expression of type INTEGER) indicates what type of call is being
done: "0" implies a "first-point" call, "1" implies a "vector" call, and
"2" implies a "last-point" or "buffer-flush" call.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
Use "CALL DPDRAW(XCPF,YCPF,0)" to do a "pen-up" move to the first point
in a sequence of points defining a curve.
.sp
Use "CALL DPDRAW(XCPF,YCPF,1)" to do "pen-down" moves to the second and
following points in a sequence of points defining a curve.
.sp
Use "CALL DPDRAW(0.,0.,2) to terminate a sequence of calls, finish drawing
the curve, and flush internal buffers.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tdshpk.
.SH ACCESS
To use DPDRAW or c_dpdraw, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashpack,
dashpack_params,
dpcurv,
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
