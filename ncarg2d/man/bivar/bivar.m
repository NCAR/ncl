.TH Bivar 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Bivar - Provides bivariate interpolation and smooth surface
fitting for values given at irregularly distributed points.
The resulting interpolating function and its first-order
partial derivatives are continuous.  The method employed is
local, i.e. a change in the data in one area of the plane does
not affect the interpolating function except in that local
area.  Also, the method gives exact results when all points lie
in a plane.
.SH SYNOPSIS
.IP IDBVIP 12 
Produces interpolated values at points (XI(I), YI(I)),
I=1,...,NIP.  This is useful for filling in missing
data points on a grid.
.IP IDSFFT 12
Performs smooth surface fitting when the projections
of the data points in the X-Y plane are irregularly
distributed in the plane.
.SH C-BINDING SYNOPSIS
c_idbvip, 
.br
c_idsfft
.SH EXAMPLES
To use Bivar routines, run "ncargex cbex01".  This will produce
several files in your directory, cbex01, cbex01.f, and cbex01.ncgm.
Edit cbex01.f and remove every line before the
line "C PACKAGE BIVAR".  What remains is the Bivar package.
.SH ACCESS 
To use the Bivar routines, load the NCAR Graphics libraries ncarg, ncarg_gks, 
ncarg_c, and ncarg_loc, preferably in that order.  To use the Bivar C-bindings,
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
idbvip, idsfft, ncarg_cbind
.SH ACKNOWLEDGMENTS
Bivar was written by Hiroshi Akima in august 1975 and rewritten
by him in late 1976.  In 1989 a new version of Bivar,
incorporating changes described in a Rocky Mountain Journal of
Mathematics was obtained from Dr. Akima, and included in NCAR
Graphics with his permission.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
