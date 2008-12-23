.TH MXMY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MXMY -
Retrieves the current pen position in the Plotter Address Unit (PAU)
coordinate system.
.SH STATUS
Plotter Address Units (PAUs) are not being used in new NCAR Graphics
routines; thus, MXMY is considered an obsolete routine.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
document "NCAR Graphics Fundamentals, UNIX Version" for a description
of these coordinate systems.
.sp
MXMY continues to be provided for compatibility of early NCAR Graphics
codes.  Frequently, consecutive calls to FRSTPT and MXMY were used to
effectively convert from user coordinates to plotter coordinates; for
this purpose, the functions KUPX and KUPY may be used instead.  If you
are writing new code, you could keep track of the pen location through
internal bookkeeping.  Better, though, would be to use fractional
coordinates for the purposes to which plotter coordinates were put; the
functions CFUX, CFUY, CUFX, and CUFY allow for conversions between user
coordinates and fractional coordinates.
.sp
The following definition of the PAU Coordinate System is included
for the purpose of interpreting and converting PAU codes:
.sp
The plotter coordinates of a point are integers IPX and IPY, where
IPX is between 1 and 2**MX and IPY is between 1 and 2**MY.  MX and
MY are internal parameters of SPPS; each has the default value 10.
Values of MX and MY can be set by routine SETI and retrieved by
routine GETSI.
.SH SYNOPSIS
CALL MXMY (IX,IY)
.SH DESCRIPTION 
.IP IX 12
(an output variable of type INTEGER) is the X coordinate of the pen
position in PAUs.
.IP IY 12
(an output variable of type INTEGER) is the Y coordinate of the pen
position in PAUs.
.SH USAGE
The routines CURVE, FRSTPT, LINE, PLOTIF, PLOTIT, POINT, POINTS, and VECTOR
update the pen position for subsequent MXMY retrieval.  ALso, the
routines PWRIT and WTSTR leave the pen position at the location
specified by their first two arguments.
.SH ACCESS
To use MXMY, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
curve, frstpt, line, plotit, point, points, pwrit, vector, seti, getsi,
cpfx, cpfy, cpux, cpuy, kfpx, kfpy, kpmx, kpmy, kmpx, kmpy, kupx, kupy,
spps, spps_converters
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
