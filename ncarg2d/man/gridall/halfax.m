.\"
.\"	$Id: halfax.m,v 1.1 1993-03-11 16:26:46 haley Exp $
.\"
.TH HALFAX 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
HALFAX - Draws orthagonal axes intersecting at a specified point
and with a specified set of labels.
.SH SYNOPSIS
CALL HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,IXLB,IYLB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_halfax (int mjrx, int mnrx, int mjry, int mnry, float xint, float yint, int ixlb, int iylb)
.SH DESCRIPTION 
.IP MJRX 12
Specifies the number of major divisions in the X axes.
.IP MNRX 12
Determines the number of minor divisions in the X axes.
.IP MJRY 12
Specifies the number of major divisions in the Y axes.
.IP MNRY 12
Determines the number of minor divisions in the Y axes.
.PP
The above parameters have different meanings, depending
on the current setting of the linear/log flag in SPPS.
This flag is set in the final parameter in the SET
call.
.sp
If the axis is linear: MJRx specifies the number of
major divisions of the X/Y axis and MNRx specifies the
number of minor divisions within each major division.
In each case, the value specifies the number of spaces
between HALFAX lines or ticks rather than the number of
lines or ticks. There is always one more major division
line or mark than the number of major divisions
specified by MJRx.  Similarly, there is always one less
minor division line or tick per major division than the
number of minor divisions per major division specified
by MNRx.
<<<Question: should MNRx be MNRX? if not we need to explain
this convention.<<<               
.sp
If the axis is logarithmic: Each major division point
occurs at a value 10**MJRx times the previous point.
For example, if the minimum X-axis value were 3., the
maximum X-axis value 3000., and MJRX 1, then the major
division points would be 3., 30., 300., and 3000. If
MNRx.LE.10, there are nine minor divisions within each
major division.  For example, between 3. and 30., there
would be minor division points at 6., 9., 12., . . .
27.  If MNRx.GT.10., minor divisions are omitted.
.IP XINT,YINT 12 
The user "world" coordinates of the point of
intersection of the two axes if IGPH equals 10. For
other values of IGPH for which one of the axes is of
type HALFAX, XINT and/or YINT specify the position of
that axis.
.IP IXLB 12
Specifies whether X axis and X-axis lbales are drawn.  Set the
value of IXLB to a real number as follows:
.RS
.IP -1 4 
X axis is not drawn.  X-axis labels are not drawn.
.IP 0 4
X axis is drawn.  X-axis labels are not drawn.
.IP 1 4
X axis is drawn.  X-axis labels are drawn.
.RE
.IP IYLB 12
Specifies whether Y axis and Y-axis labels are drawn.  Set the
value of IYLB to a real number as follows:
.RS
.IP -1 4
Y axis is not drawn.  Y-axis is not labels.
.IP 0 4
Y axis is drawn.  Y-axis is not labels.
.IP 1 4
Y axis is drawn.  Y-axis is labels.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH USAGE
The statement:
.RS 3 
.sp
CALL HALFAX (MJRX,MNRX,MJRY,MNRY,XINT,YINT,IXLB,IYLB)
.sp
.RE
is equivalent to 
.RS 3
.sp
CALL GRIDAL (MJRX,MNRX,MJRY,MNRY,IXLB,IYLB,10,XINT,YINT)
.RE
.SH ACCESS
To use HALFAX, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_halfax, load 
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
