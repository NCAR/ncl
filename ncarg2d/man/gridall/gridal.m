.\"
.\"	$Id: gridal.m,v 1.1 1993-03-11 16:26:41 haley Exp $
.\"
.TH GRIDAL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GRIDAL - Draws a grid, either labeled or unlabeled, with or
without axes and a perimeter.
.SH SYNOPSIS
CALL GRIDAL (MJRX, MNRX, MJRY, MNRY, IXLB, IYLB, IGPH, XINT, YINT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gridal (int mjrx, int mnrx, int mjry, int mnry, int ixlb, int iylb, int igph, float xint, float yint)
.SH DESCRIPTION 
.IP "MJRX" 12
(Integer, Input) - 
The number of major divisions in the X 
axes.
.IP "MNRX" 12
(Integer, Input) - 
Determines the number of minor 
divisions in the X axes.
.IP "MJRY" 12
(Integer, Input) - 
The number of major divisions in the Y 
axes.
.IP "MNRY" 12
(Integer, Input) - 
Determines the number of minor 
divisions in the Y axes.
.sp
The preceding parameters have different meanings, 
depending on the current setting of the linear/log flag in 
SPPS. This flag is set in the final parameter in the SET 
call.
.sp
If the axis is linear, MJRX specifies the number of major 
divisions of the X/Y axis and MNRX specifies the number 
of minor divisions within each major division. In each 
case, the value specifies the number of spaces between 
GRIDAL lines or ticks rather than the number of lines or 
ticks. There is always one more major division line or 
mark than the number of major divisions specified by 
MJRX. Similarly, there is always one less minor division 
line or tick per major division than the number of minor 
divisions per major division specified by MNRX.
.sp
If the axis is logarithmic, each major division point 
occurs at a value 10**MJRX times the previous point. For 
example, if the minimum X-axis value is 3., the maximum 
X-axis value is 3000., and MJRX is 1, then the major 
division points would be 3., 30., 300., and 3000. If 
MNRX.LE.10, there are nine minor divisions within each 
major division. For example, between 3. and 30., there 
would be minor division points at 6., 9., 12., . . . 27. If 
MNRX.GT.10., minor divisions are omitted.
.IP "IXLB" 12
(Integer, Input) - 
Specifies X-axis appearance.
.RS
.IP "-1" 4
No X axis drawn.  No X-axis labels.
.IP "0" 4
X axis drawn.  No X-axis labels.
.IP 1 4
X axis drawn.  X-axis labels.
.RE
.IP "IYLB" 12
(Integer, Input) - 
Specifies Y-axis appearance.
.RS
.IP "-1" 4
No axis drawn.  No Y-axis labels.
.IP "0" 4
Y axis drawn.  No Y-axis labels.
.IP "1" 4
Y axis drawn.  Y-axis labels.
.RE
.IP "IGPH" 12
(Integer, Input) - 
Specifies the background type as indicated by one
of the following integers:
.sp
.TS
tab (/);
c c c
l l l .
IGPH/X axis/Y axis
.sp
0/grid/grid
1/grid/perimeter
2/grid/axis
4/perimeter/grid
5/perimeter/perimeter
6/perimeter/axis
8/axis/grid
9/axis/perimeter
10/axis/axis
.TE
.IP "XINT,YINT" 12
(Real, Input) - 
The user "world" coordinates of the point of 
intersection of the two axes if IGPH equals 10. For other 
values of IGPH for which one of the axes is the axis type, 
XINT and/or YINT specifies the position of that axis.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use GRIDAL, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_gridal, load
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gacolr, gagetc, gageti, gagetr, gasetc, gaseti, gasetr, grid, gridal,
gridl, halfax, labmod, perim, periml, tick4, ticks, ncarg_cbind
.sp
Hardcopy: "NCAR Graphics User's Guide," Version 2.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

