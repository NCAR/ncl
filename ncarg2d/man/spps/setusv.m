.TH SETUSV 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
.na
.nh
SETUSV - sets the value of one of the spps parameters.
.SH SYNOPSIS
CALL SETUSV (VN,IV)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_setusv (char *vn, int iv)
.SH DESCRIPTION 
.IP VN 12
(an input parameter name of type CHARACTER) consists of a two letter name
contained within apostrophes.
.IP IV 12
(an input parameter value of type INTEGER) taken from the following parameter
table:
.nf
Parameter       Description                Values(IV)
Name (VN)

 'LS'            Axis scaling
					    1 = linear X, linear Y
					    2 = linear X, log Y
					    3 = log X,    linear Y
					    4 = log X,    log Y

 'MI'
		 Axis direction reversal    1 = neither X nor Y
		 (The designated axes run   2 = X, but not Y
		 from maximum to minimum    3 = Y, but not X
		 rather that min to max)    4 = both X and Y

 'PB'            Pen-move buffer size       2 to 50

.fi
Both 'LS' and 'MI' are normally set by a call to routine SET.

The parameter 'PB' sets the buffer size used in routine PLOTIF.
.nf

Obsolete        Description
parameters

 'XF'            X axis exponent for plotter coordinates
		 (The PAU range would be 1 to 2**XF - 1 in X)
 'YF'            Y axis exponent for plotter coordinates
		 (The PAU range would be 1 to 2**YF - 1 in Y)
 'MU'            Metacode output unit
 'IR'            Red color intensity
 'IG'            Green color intensity
 'IB'            Blue color intensity
 'IM'            Overall color intensity
 'IN'            Maximum color index
 'II'            Restore color index
 'LW'            Line width scale factor in thousandths
		 (2000 means double the default line width)
 'MS'            Marker size in thousandths
		 (2000 means double the default marker size)

.fi
The values of parameters XF and YF are the same as the values of
arguments IX and IY in a call to routines SETI, or GETSI.
.sp
Parameter MU was used to set a FORTRAN unit number for a file to
receive metacode output.
.sp
Parameters IR, IG, IB, IM, IN, II were all part of an obsolete color
setting scheme meant to allow for variable plotting intensities.  This
process has been replaced by normal GKS color setting options through
the routines GSCR, GSPLCI, GSFACI, GSPMCI, and GSTXCI.  See Section 6.
of "User's Guide for NCAR GKS-0A Graphics, Version 2.0."
.sp
Parameter LW is replaced by the GKS line width scale factor routine, GSLWSC,
which has the corresponding query function GQLWSC.
.sp
Parameter MS is replaced by the GKS marker size scale factor routine, GSMKSC,
which has the corresponding query function GQMKSC.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
(none).
.SH ACCESS
To use SETUSV, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_setusv, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gscr,gsplci, gsfaci, gspmci, gstxci, gsmksc, gslwsc,
spps, spps_params, getset, getsi, getusv, plotif, set, setsi, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
