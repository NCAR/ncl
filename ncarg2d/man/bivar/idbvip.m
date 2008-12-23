.TH IDBVIP 3NCARG "November 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
IDBVIP - produces interpolated values at points (XI(I), YI(I)),
I=1,...,NIP.  This is useful for filling in missing data points
on a grid.
.SH SYNOPSIS
CALL IDBVIP (MD,NDP,XD,YD,ZD,NIP,XI,YI,ZI,IWK,WK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_idbvip (int md, int ndp, float *xd, float *yd, 
.br
float *zd, int nip, float *xi, float *yi, float *zi, 
.br
int *iwk, float *wk)
.SH DESCRIPTION
.IP MD 12
Mode of computation (must be 1, 2, or 3, else an error
return occurs.)
.RS
.IP 1 
If this is the first call to this subroutine, or if
the value of NDP has been changed from the previous
call, or if the contents of the XD or YD arrays have
been changed from the previous call.
.IP 2 
If the values of NDP and the XD and YD arrays are
unchanged from the previous call, but new values for
XI, YI are being used.  If MD = 2 and NDP has been
changed since the previous call to IDBVIP, an error
return occurs.
.IP 3 
If the values of NDP, NIP, XD, YD, XI, YI are
unchanged from the previous call, i.e. if the only
change on input to IDBVIP is in the ZD array.  If MD=3
and NDP or NIP has been changed since the previous call
to IDBVIP, an error return occurs.
.RE
.IP ""
Between the call with MD=2 or MD=3 and the preceding
call, the IWK and WK work arrays should not be
disturbed.
.IP NDP 12
Number of data points (must be 4 or greater, else an
error return occurs).
.IP XD 12
Array of dimension NDP containing the X coordinates of
the data points.
.IP YD 12
Array of dimension NDP containing the Y coordinates of
the data points.
.IP ZD 12
Array of dimension NDP containing the Z coordinates of
the data points.
.IP NIP 12
The number of output points at which interpolation is
to be performed (must be 1 or greater, else an error
return occurs).
.IP XI 12
Array of dimension NIP containing the X coordinates of
the output points.
.IP YI 12
Array of dimension NIP containing the Y coordinates of
the output points.
.IP IWK 12
Integer work array of dimension at least 31*NDP + NIP.
.IP WK 12
Real work array of dimension at least 8*NDP.
.IP ZI 12
Array of dimension NIP where interpolated Z values are
to be stored.
.SH ""
Inadequate work space IWK and WK may may cause incorrect results.
.sp
The data points must be distinct and their projections in the
X-Y plane must not be collinear, otherwise an error return
occurs.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
To use Bivar routines, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH ACCESS 
To use IDBVIP or c_idbvip, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the bivar man page for a description of all Bivar error
messages and/or informational messages.
.SH SEE ALSO
Online:
bivar, bivar_params, idsfft, idpltr, idgeti, idgetr, idseti, idsetr,
ncarg_cbind
.SH ACKNOWLEDGMENTS
Bivar was written by Hiroshi Akima in August 1975 and rewritten
by him in late 1976.  In 1989, a new version of Bivar,
incorporating changes described in a Rocky Mountain Journal of
Mathematics was obtained from Dr. Akima, and included in NCAR
Graphics with his permission.  In 1995, Dave Kennison incorporated
the capability of doing linear interpolation and a different kind
of triangulation, put in a parameter access interface, and wrote a
routine to allow the triangulation to be plotted.
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
