.TH CONRAS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CONRAS - draws a two dimensional contour map from irregularly
distributed data.  This is the super version of the Conran_family.
.SH STATUS
CONRAS is approaching obsolescence.  If you have available an
interpolation package which can do a random to gridded interpolation
equal to that built into the Conran_family, then we recommend that
you grid your dataset and use the Conpack utility instead.
Refer to the conpack and
conpack_params man pages as well as the NCAR Graphics
Contouring and Mapping Tutorial.
.SH SYNOPSIS
CONRAS (XD,YD,ZD,NDP,WK,IWK,SCRARR)
.SH DESCRIPTION 
.IP XD 12
Input, real array -- of dimension NDP containing the X
coordinates of the data points.
.IP YD 12
Input, real array -- of dimension NDP containing the Y
coordinates of the data points.
.IP ZD 12
Input, real array -- of dimension NDP containing the
data values at the points.
.IP NDP 12
Input, integer -- is the number of data points
to be contoured.  NDP must be 4 or larger.
.IP WK 12
Input, real array -- workspace of dimension
15*NDP.
.IP IWK 12
Input, integer array -- workspace which must be dimensioned at
least IWK((27+NCP)*NDP).
.sp
The parameter NCP controls the
number of data points to be used in the
interpolation.  Increasing NCP causes more
of the surrounding data to influence the
point of interpolation.  In the case of linear interpolation
NCP is always 4.  In the case of C1 interpolation, NCP
can vary from 2 to 25 with 4 as the default.
.sp
The interpolation option is selected using parameter ITP.
See the
conran_family_params man page for a description of all internal parameters.
.IP SCRARR 12
Real work array of dimension at least
(RESOLUTION**2) where RESOLUTION is
40 by default.
.sp
Alternate resolutions are set using parameter SSZ.  See the
conran_family_params man page.
.SH USAGE
The contours will be drawn as smoothed
dashed or solid lines using splines under tension.  There may be
characters along the lines.  Crowded contour lines may be thinned.
Many other options are available through the use of internal parameters.
See the conran_family_params man page.
.sp
Quick and standard-and-smooth versions of the Conran_family are also available
through entries CONRAQ and CONRAN, respectively.
.SH EXAMPLES
Use the ncargex command to see the relevant example tconas.
.SH ACCESS 
To use CONRAS, load the NCAR Graphics libraries conras, dashsupr,
ncarg, ncarg_gks, and ncarg_c, preferably in that order.
.sp
The conras and dashsupr libraries are not loaded as part of the normal NCAR Graphics
installation.  They can be loaded by the user directly, or loaded implicitly
through the use of the -super option of the ncargf77 command.
.sp 2
.IP SUPER 10
Command: "ncargf77 -super mycode.f"
.SH MESSAGES
See the conran_family man page for a description of all Conran_family
error messages and/or informational messages.
.SH SEE ALSO
Online:
conraq, conran, conran_family_params, conran_family, conop1, conop2, conop3,
conop4, conpack, conpack_params, bivar,
dashline_family, ncargf77
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
