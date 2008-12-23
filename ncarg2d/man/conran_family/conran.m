.TH CONRAN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CONRAN - draws a two dimensional contour map from irregularly
distributed data.  The contour lines may be smoothed.
.SH STATUS
CONRAN is approaching obsolescence.  If you have available an
interpolation package which can do a random to gridded interpolation
equal to that built into the Conran_family, then we recommend that
you grid your dataset and use the Conpack utility instead.
Refer to the conpack and
conpack_params man pages as well as the NCAR Graphics
Contouring and Mapping Tutorial.
.SH UTILITY
This routine is part of the Conran_family in NCAR Graphics.  To see
the overview man page for this utility, type "man conran_family".
.SH SYNOPSIS
CONRAN (XD,YD,ZD,NDP,WK,IWK,SCRARR)
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
characters along the lines.  Many other
options are available through the use of internal parameters.
See the conran_family_params man page.
.sp
Quick and super versions of the Conran_family are also available
through entries CONRAQ and CONRAS, respectively.
.SH EXAMPLES
Use the ncargex command to see the relevant example tconan.
.SH ACCESS 
CONRAN can be invoked in two different ways.  The standard version produces
contour plots which connect interpolated values on the boundary of the
interpolation triangles.  The smooth version produces contours which
are smoothed using splines under tension.
.sp
To use standard CONRAN, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  To run the smooth option an
additional library, dashsmth is needed.
It can be loaded by the user directly, along with the above three
standard libraries, or loaded implicitly
through the use of the -smooth option of the ncargf77 command.
To run a code called mycode.f which has one or more calls to
entry CONRAN, issue one of the commands:
.sp
.IP NORMAL 10
Command: "ncargf77 mycode.f"
.sp
The contours will be drawn as unsmoothed
dashed or solid lines that can include characters along the lines.
.sp 2
.IP SMOOTH 10
Command: "ncargf77 -smooth mycode.f"
.sp
The contours will be drawn as smoothed
dashed or solid lines that can include characters along the lines.
.SH MESSAGES
See the conran_family man page for a description of all Conran_family
error messages and/or informational messages.
.SH SEE ALSO
Online:
conraq, conras, conran_family_params, conran_family, conop1, conop2, conop3,
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
