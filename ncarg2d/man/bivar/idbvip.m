.\"
.\"	$Id: idbvip.m,v 1.1 1993-03-11 16:15:12 haley Exp $
.\"
.TH IDBVIP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
IDBVIP - produces interpolated values at points (XI(I), YI(I)),
I=1,...,NIP.  This is useful for filling in missing data points
on a grid.
.SH SYNOPSIS
CALL IDBVIP (MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,IWK,WK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_idbvip (int md, int ndp, float *xd, float *yd, float *zd, int nip, float *xi, float *yi, float *zi, int *iwk, float *wk)
.SH DESCRIPTION
.IP MD 12
Mode of computation (must be 1, 2, or 3, else an error
return occurs.)
.sp
1 If this is the first call to this subroutine, or if
the value of NDP has been changed from the previous
call, or if the contents of the XD or YD arrays have
been changed from the previous call.
.sp
2 If the values of NDP and the XD and YD arrays are
unchanged from the previous call, but new values for
XI, YI are being used.  If MD = 2 and NDP has been
changed since the previous call to IDBVIP, an error
return occurs.
.sp
3 If the values of NDP, NIP, XD, YD, XI, YI are
unchanged from the previous call, i.e. if the only
change on input to IDBVIP is in the ZD array.  If MD=3
and NDP or NIP has been changed since the previous call
to IDBVIP, an error return occurs.
.sp
Between the call with MD=2 or MD=3 and the preceding
call, the IWK and WK work arrays should not be
disturbed.
.IP NDP 12
Number of data points (must be 4 or greater, else an
error return occurs).
.IP XD 12
array of dimension NDP containing the X coordinates of
the data points.
.IP YD 12
array of dimension NDP containing the Y coordinates of
the data points.
.IP ZD 12
array of dimension NDP containing the Z coordinates of
the data points.
IP NIP 12
the number of output points at which interpolation is
to be performed (must be 1 or greater, else an error
return occurs).
.IP XI 12
array of dimension NIP containing the X coordinates of
the output points.
.IP YI 12
array of dimension NIP containing the Y coordinates of
the output points.
.IP IWK 12
integer work array of dimension at least 31*NDP + NIP
.IP WK 12
real work array of dimension at least 8*NDP
.IP ZI 12
array of dimension NIP where interpolated Z values are
to be stored.
.IP SPECIAL 12
CONDITIONS     
Inadequate work space IWK and WK may may cause incorrect results.
.sp
The data points must be distinct and their projections in the
X-Y plane must not be collinear, otherwise an error return
occurs.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH EXAMPLES
To use BIVAR routines run "ncargex cbex01".  This will produce
several files in your directory, cbex01, cbex01.f, cbex01.cgm
and cbexcc.f.  Edit cbex01.f and remove every line before the
line "C PACKAGE BIVAR".  What remains is the Bivar package.
.SH ACCESS 
To use IDBVIP load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.  To use c_idbvip load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.
.SH ACKNOWLEGEMENTS
.IP BIVAR 12
was written by Hiroshi Akima in august 1975 and rewritten
by him in late 1976.  In 1989 a new version of BIVAR,
incorporating changes described in a Rocky Mountain Journal of
Mathematics was obtained from Dr. Akima, and included in NCAR
Graphics with his permission.
.SH SEE ALSO
Online:
bivar, idbvip, idsfft, ncarg_cbind
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
<<<Question: Since BIVAR was written by a non-SCD person, do
we still use the copyright notice here?<<< 
