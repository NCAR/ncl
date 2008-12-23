.TH SPPS_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SPPS_params - This document briefly describes all of the internal parameters
of SPPS.
.SH DESCRIPTION 
In calls to GETUSV and SETUSV, which are used to access the internal
parameters of SPPS, the arguments VN and IV are as follows:

.IP VN 12
(an input constant or variable of type CHARACTER) is the name of a parameter;
only the first two characters are meaningful.  Remember that, in FORTRAN, a
constant of type CHARACTER is a string of characters enclosed in apostrophes.

.IP IV 12
(an input or output value of type INTEGER), is either the returned current
value of the parameter specified by VN or the desired new value of that
parameter, as described in the following table:
.nf

Parameter   Description               Possible Values of IV
Name (VN)
---------   -----------               ---------------------
 'LS'       Axis scaling              1 = linear X, linear Y
            ("Log Scaling" flag)      2 = linear X, log Y
                                      3 = log X,    linear Y
                                      4 = log X,    log Y

 'MI'       Axis direction reversal   1 = neither X nor Y
            ("MIrroring" flag - axis  2 = X, but not Y
            from maximum to minimum   3 = Y, but not X
            rather than min to max)   4 = both X and Y

 'MU'       Metacode output Unit      2

 'PB'       Pen-move Buffer size      2 to 50

.fi
Both 'LS' and 'MI' are normally set by calling the routine SET, but
can be set independently by calling SETUSV.
.sp
The parameter 'MU' is set, prior to calling OPNGKS, to change the
FORTRAN logical unit number to be used by GKS for metacode output.
.sp
The parameter 'PB' sets the size of the SPPS polyline buffer.
The value "2" effectively turns buffering off.
.nf

Obsolete    Description
parameters
----------  -----------
 'XF'       X axis exponent for plotter coordinates
            (The PAU range would be 1 to 2**XF - 1 in X)
 'YF'       Y axis exponent for plotter coordinates
            (The PAU range would be 1 to 2**YF - 1 in Y)
 'IR'       Red color intensity (relative to green and blue)
 'IG'       Green color intensity (relative to red and blue)
 'IB'       Blue color intensity (relative to red and green)
 'IM'       Maximum color index to be used
 'IN'       Overall color intensity
 'II'       Restore color index
 'LW'       Line width scale factor in thousandths
            (2000 means double the default line width)
 'MS'       Marker size in thousandths
            (2000 means double the default marker size)

.fi
The values of the parameters 'XF' and 'YF' are the same as the values of
the arguments IX and IY in calls to the routines SETI and GETSI.
.sp
The parameters 'IR', 'IG', 'IB', 'IM', 'IN', and 'II' were all part of a
color-setting scheme meant to allow overall intensity to be set independently
of color.  This scheme is now obsolete; it has been replaced by normal GKS
color setting through the routines GSCR, GSPLCI, GSFACI, GSPMCI, and GSTXCI.
The corresponding GKS inquiry routines to determine current color settings
are GQCR, GQPLCI, GQFACI, GQPMCI, and GQTXCI.  See Section 6 of "User's Guide
for NCAR GKS-0A Graphics."
.sp
The parameter 'LW' is replaced by the GKS line width scale factor routine,
GSLWSC, which has the corresponding query function GQLWSC.
.sp
The parameter 'MS' is replaced by the GKS marker size scale factor routine,
GSMKSC, which has the corresponding query function GQMKSC.
.SH USAGE
Values of SPPS parameters are set by calling the routine SETUSV and retrieved
by calling the routine GETUSV.
.SH SEE ALSO
Online:
gscr,gsplci, gsfaci, gspmci, gstxci, gsmksc, gslwsc,
spps, getset, getsi, getusv, plotif, set, setsi, setusv
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
