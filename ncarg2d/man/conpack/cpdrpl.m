.TH CPDRPL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPDRPL - 
Provides a useful polyline-drawer for the
routine CPCLDM.
.SH SYNOPSIS
CALL CPDRPL (XCS, YCS, NCS, IAI, IAG, NAI)
.SH DESCRIPTION 
.IP XCS 12
(a REAL array of dimension at least NCS, input) is an 
array containing the X coordinates of NCS points defining a 
polyline.
.IP YCS 12
(a REAL array of dimension at least NCS, input) is an 
array containing the Y coordinates of NCS points defining a 
polyline.
.IP NCS 12
(INTEGER, input) is the number of points defining the 
polyline.
.IP IAI 12
(an INTEGER array of dimension at least NAI, input) is 
an array of area identifiers for the area in which the 
polyline lies. For each I from 1 to NAI, IAI(I) is the area 
identifier of the area with respect to the edge group 
IAG(I).
.IP IAG 12
(an INTEGER array of dimension at least NAI, input) is 
an array of group identifiers. See the description of IAI, 
above.
.IP NAI 
(INTEGER, input) is the number of area identifiers in 
the array IAI and the number of group identifiers in the 
array IAG.
.SH USAGE
If CPCLDM is called, and the only object of using it,
instead of CPCLDR, to draw contour lines is to avoid
drawing the lines through labels, then, in the routine
which calls CPCLDM, put the declaration
.sp
.RS 5
EXTERNAL CPDRPL
.RE
.sp
and, in the call to CPCLDM, use CPDRPL for the argument
RTPL. Each time CPDRPL is called, it draws the polyline
defined by its first three arguments if, and only if, none
of the area identifiers defined by the other three
arguments is negative.
.SH ACCESS
To use CPDRPL, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
