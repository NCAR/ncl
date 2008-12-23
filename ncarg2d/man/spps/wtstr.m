.TH WTSTR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
WTSTR - draws a specified text string at a specified position in the
user coordinate system.  The call has arguments specifying
the size, orientation, and centering of the string.
.SH STATUS
WTSTR specifies character sizes in
Plotter Address Units (PAUs), which are no longer used in NCAR Graphics;
thus, WTSTR is considered an obsolete routine.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
document "NCAR Graphics Fundamentals, UNIX Version" for a description
of these coordinate systems.
.sp
WTSTR continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use the more
general text writing routines
of the Plotchar utility:  PLCHLQ, PLCHMQ, and PLCHHQ.
.sp
The following definition of the PAU Coordinate System is included
for the purpose of interpreting and converting PAU codes:
.sp
The plotter coordinates of a point are integers IPX and IPY, where
IPX is between 1 and 2**MX and IPY is between 1 and 2**MY.  MX and
MY are internal parameters of SPPS; each has the default value 10.
Values of MX and MY can be set by the routine SETI and retrieved by
the routine GETSI.
.SH SYNOPSIS
CALL WTSTR(PX,PY,CH,IS,IO,IC)
.SH DESCRIPTION 
.IP PX 12
(an input expression of type REAL) defines the X user coordinate
where the text string is to be drawn.
.IP PY 12
(an input expression of type REAL) defines the Y user coordinate
where the text string is to be drawn.
.IP CH 12
(an input constant or variable of type CHARACTER) is the text
string to be drawn.
.IP IS 12
(an input expression of type INTEGER) specifies the character
width in plotter address units (PAUs).
.IP IO 12
(an input expression of type INTEGER) specifies the rotation angle of
the text string.  IO is measured in degrees counter-clockwise from a
horizontal orientation.
.IP IC 12
(an input expression of type INTEGER) specifies the centering
option, as follows:
.RS
.IP "<0" 4
(PX,PY) will be in the vertical center
of the left edge of the leftmost character
in the original horizontal string.
.IP "=0" 4
(PX,PY) will be in the center of the
text extent rectangle.
.IP ">0" 4
(PX,PY) will be in the vertical center
of the right edge of the rightmost character
in the original horizontal string.
.RE
.PP
A call to WTSTR causes the SPPS polyline buffer to be flushed and leaves
the "pen" (for subsequent calls to FRSTPT, VECTOR, PLOTIF, and PLOTIT)
at the location (PX,PY).
.SH ACCESS
To use WTSTR, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gtx, gstxal, gstxp, gstxfp, gschh, gschsp, gschup, gschxp, gscr, gstxci,
spps, plotchar, plchhq, plchmq, plchlq, pwrit, plotif, seti, getsi
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
