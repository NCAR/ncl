.TH PWRIT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PWRIT - draws a specified text string at a specified position in the
user coordinate system.  The call has arguments specifying
the size, orientation, and centering of the string.
.SH STATUS
PWRIT is an earlier version of the routine WTSTR in which the number
of characters in the text string was specified in an argument.  Both
WTSTR and PWRIT are now considered obsolete.
.sp
PWRIT continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use the more
general text writing routines
of the Plotchar utility:  PLCHLQ, PLCHMQ, and PLCHHQ.
.SH SYNOPSIS
CALL PWRIT (PX,PY,CH,NC,IS,IO,IC)
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
.IP NC 12
(an input expression of type INTEGER) specifies the number of
characters in the 'CH' text string.
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
A call to PWRIT causes the SPPS polyline buffer to be flushed and leaves
the "pen" (for subsequent calls to FRSTPT, VECTOR, PLOTIF, and PLOTIT)
at the location (PX,PY).
.SH ACCESS
To use PWRIT, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gtx, gstxal, gstxp, gstxfp, gschh, gschsp, gschup, gschxp, gscr, gstxci,
spps, plotchar, plchhq, plchmq, plchlq, wtstr, plotif, seti, getsi
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
