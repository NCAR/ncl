.TH DPSMTH 3NCARG "March 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DPSMTH - Used to draw a curve when fractional coordinates are available and
smoothing is to be done.
.SH SYNOPSIS
CALL DPSMTH (XCPF,YCPF,IFVL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dpsmth (float xcpf, float ycpf, int ifvl);
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
Use "CALL DPSMTH(XCPF,YCPF,0)" to do a "pen-up" move to the first point
in a sequence of points defining a curve.
.sp
Use "CALL DPSMTH(XCPF,YCPF,1)" to do "pen-down" moves to the second and
following points in a sequence of points defining a curve.
.sp
Use "CALL DPSMTH(0.,0.,2) to terminate a sequence of calls, finish drawing
the curve, and flush internal buffers.
.sp
DPSMTH accumulates points until it has an internal buffer-load or until the
last one has been received.  It then creates a smooth curve passing through
all the points, interpolates points along that smooth curve, and passes those
points along to the routine DPDRAW.  If the internal parameter 'TCS' has a
value less than or equal to zero, the smoothing is done using simple cubic
splines; if 'TCS' has a value greater than zero, the smoothing is done using
cubic splines under tension and 'TCS' specifies the desired tension.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tdshpk.
.SH ACCESS
To use DPSMTH or c_dpsmth, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
dplast,
dpline,
dpsetc,
dpseti,
dpsetr,
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
