.TH PWRIT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PWRIT - draws a text string 'CH' at the location (PX,PY) in the
user coordinate system.  The call contains
options on the length, size, orientation, and centering of the string.
.SH STATUS
PWRIT is an earlier version of the WTSTR routine in which the number
of characters in the text string was specified in an argument.  Since
WTSTR is now obsolete, PWRIT is also obsolete.
.sp
PWRIT continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use the more
general text writing routines
of the Plotchar utility, PLCHLQ, PLCHMQ, and PLCHHQ.
.SH SYNOPSIS
CALL PWRIT (PX,PY,CH,NC,IS,IO,IC)
.SH DESCRIPTION 
.IP PX 12
(an input coordinate of type REAL) defining the X user coordinate
where the text string is to be drawn.
.IP PY 12
(an input coordinate of type REAL) defining the Y user coordinate
where the text string is to be drawn.
.IP CH 12
(an input text string of type CHARACTER) to be drawn.
.IP NC 12
(an input parameter of type INTEGER) which specifies the number of
characters in the 'CH' text string.
.IP IS 12
(an input parameter of type INTEGER) which specifies the character
width in plotter address units (PAUs).
.IP IO 12
(an input parameter of type INTEGER) which specifies the rotation in
degrees of the text string in the counter-clockwise direction
starting from a horizontal orientation.
.IP IC 12
(an input parameter of type INTEGER) which specifies the centering
option according to:
.nf

IC < 0, (PX,PY) will be in the vertical center
	of the left edge of the leftmost character
	in the original horizontal string.

IC = 0, (PX,PY) will be in the center of the
	text extent rectangle.

IC > 0, (PX,PY) will be in the vertical center
	of the right edge of the rightmost character
	in the original horizontal string.

.fi
A call to PWRIT causes the PLOTIF buffer to be flushed and leaves
the drawing pen at location (PX,PY)
.SH ACCESS
To use PWRIT, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gtx, gstxal, gstxp, gstxfp, gschh, gschsp, gschup, gschxp, gscr, gstxci,
spps, plotchar, plchhq, plchmq, plchlq, wtstr, plotif, seti, getsi
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
