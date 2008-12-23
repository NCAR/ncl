.TH Dashline_params 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Dashline_params - Brief descriptions of all internal parameters of Dashline.
.SH DESCRIPTION 
No parameter-access routines are available for the utility Dashline.
Parameters are set through the common blocks SMFLAG, INTPR, and DASHD1.
.sp
There are four different versions of Dashline: normal, quick, smooth, and
super.  Which version is used depends on options used on the ncargf77
command line.  The lines drawn by the different versions can vary
considerably in appearance.
.sp
The following set of common blocks may be used to access any of the internal
parameters of the Dashline utility:
.IP " " 2
 COMMON /SMFLAG/ IOFFS
.sp
 COMMON /INTPR / IPAU,FPART,TENSN,NP,SMALL,L1,ADDLR,ADDTB,MLLINE,
.br
+                ICLOSE
.sp
 COMMON /DASHD1/ ISL,L,ISIZE,IP(100),NWDSM1,IPFLAG(100),MNCSTR,IGP
.PP
The following table indicates which of the internal parameters affect the
behavior of the various versions of Dashline ("Y" indicates an effect, "N"
indicates no effect):
.IP " " 2
.TS
tab(/);
l c c c c.
Parameter/QUICK/NORMAL/SMOOTH/SUPER
---------/-----/------/------/-----
ADDLR/N/N/N/Y
ADDTB/N/N/N/Y
FPART/N/Y/Y/Y
ICLOSE/Y/Y/Y/Y
IGP/N/Y/Y/Y
IOFFS/N/N/Y/Y
IPAU/Y/Y/Y/Y
L1/N/N/Y/Y
MLLINE/N/N/N/Y
NP/N/N/Y/Y
SMALL/N/N/Y/Y
TENSN/N/N/Y/Y
.TE
.PP
PARAMETER DESCRIPTIONS
.IP ADDLR 12
The amount of free space to be left on the left and on the right of each
character string, stated as the desired number of NDCs (Normalized Device
Coordinates) times 1023.  This only affects the "super" version of Dashline;
it changes the size of the area marked around a label in the bit map that
is used for culling crowded lines.
[Default = 2. (2./1023. NDCs)]
.IP ADDTB 12
The amount of free space to be left on the top and on the bottom of each
character string, stated as the desired number of NDCs (Normalized Device
Coordinates) times 1023.  This only affects the "super" version of Dashline;
it changes the size of the area marked around a label in the bit map that
is used for culling crowded lines.
[Default = 2. (2./1023. NDCs)]
.IP FPART 12
A multiplication factor for the length of the first solid line segment on any
curve.  This can be used to offset labels.  For example, if FPART = \.5, the
first solid line segment is only one-half as long as the other solid line
segments.  This will move all labels on the current curve towards the
beginning of the curve and thereby reduce the probability of a label being
written on top of a label on a nearby curve drawn with FPART = 1.
[Default = 1.]
.IP ICLOSE 12
An internal or external call to move the "drawing pen" (pen-up) to a specific
position is executed only if the position is more than ICLOSE units away
from the current pen position (the distance is measured as the absolute
difference in X-coordinates plus the absolute difference in Y-coordinates).
The unit of measurement is 32767 integer units in each of the X and Y
dimensions.
[Default = 6]
.IP IGP 12
Flag to control whether gaps are left for labels along a curve.  If no gaps
are left, the labels don't look as good, but you can be sure of what the
curve is doing in the regions occupied by the labels.
.RS
.IP 9 3
A gap is left.
.IP 0 3
No gap is left.
.RE
.IP " " 12
[Default = 9]
.IP IOFFS 12
Flag used to turn smoothing off.
.RS
.IP 0 3
Smoothing.
.IP 1 3
No smoothing.
.RE
.IP " " 12
[Default = 0]
.IP IPAU 12
The length of the gap or solid line associated with each element of a dash
pattern defined by a call to DASHDB, given as the desired value in NDCs
(Normalized Device Coordinates) times 1023.  Such a dash pattern is repeated
approximately every REAL(IPAU)/64. NDC units along the length of a curve.
[Default = 3 (3./1023. NDCs)]
.IP L1 12
The maximum number of points to be saved at one time.  If there are more than
L1 points on a given line, L1 points are processed, then the next L1, and so
on, until the entire line is processed.  Smoothness between segments is
maintained automatically.  If L1 is increased, the dimensions of XSAVE,
YSAVE, XP, YP, TEMP, and SLEN in the routine FDVDLD must be increased to
the new value of L1.
[Default = 70]
.IP MLLINE 12
The maximum length in either coordinate of a single line segment to be
processed by the crowded line algorithm.  Line segments longer than MLLINE
units in either coordinate are broken into smaller segments, each of which
is processed separately.  This is done to prevent anomalies in the removal
of long line segments, since only the starting point and end point of each
line segment is checked in that process.  The unit of measurement is 32767
integer units in each of the X and Y dimensions.
[Default = 384]
.IP NP 12
Determines how many points are used to draw each smoothed curve.  The value
of NP is twice the maximum number of interpolated points on a horizontal
curve with length equal to that of the width of the grid.  More points per
unit length are interpolated for short lines than for long lines.
[Default = 150]
.IP SMALL 12
When the points on a line are being processed, only the first of two
consecutive points is saved if the points are less than SMALL units apart.
This procedure is to prevent cusps.  The unit of measurement is 32767
integer units in each of the X and Y dimensions.
[Default = 128.]
.IP TENSN 12
Tension factor.  Must be greater than 0.  A large tension factor (30.) would
essentially turn off smoothing.
[Default = 2.5]
.SH SEE ALSO
Online:
dashline, curved, dashdb, dashdc, frstd,
lastd, lined, reset, vectd, ncarg_cbind
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
