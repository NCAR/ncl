.TH SETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SETI - sets the exponents which define the plotting range
in the plotter address unit coordinate system.
.SH STATUS
Plotter Address Units (PAUs) are no longer used in NCAR Graphics;
thus, SETI is considered an obsolete routine.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
document "NCAR Graphics Fundamentals, UNIX Version" for a description
of these coordinate systems.
.sp
SETI continues to be provided for compatibility of early NCAR Graphics
codes.
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
CALL SETI (IX,IY)
.SH DESCRIPTION 
.IP IX 12
(an input expression of type INTEGER) is the X axis exponent for plotter
coordinates defining a range of 1 to 2**IX - 1.  IX has a default value
of 10.
.IP IY 12
(an input expression of type INTEGER) is the Y axis exponent for plotter
coordinates defining a range of 1 to 2**IY - 1.  IY has a default value
of 10.
.SH ACCESS
To use SETI, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
spps, getsi
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
