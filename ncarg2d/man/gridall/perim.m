.\"
.\"	$Id: perim.m,v 1.1 1993-03-11 16:26:49 haley Exp $
.\"
.TH PERIM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
PERIM - To draw a labeled perimeter with inward pointing tick
marks.  The directions and lengths of tick marks may be changed
by calling TICKS and/or TICK4
.SH SYNOPSIS
CALL PERIM (MJRX, MNRX, MJRY, MNRY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_perim (int mjrx, int mnrx, int mjry, int mnry)
.SH DESCRIPTION 
.IP MJRX 12
the number of major divisions in the X axes
.IP MNRX 12
determines the number of minor divisions in the X axes
.IP MJRY 12
the number of major divisions in the Y axes
.IP MNRY 12
determines the number of minor divisions in the Y axes
.sp
These parameters have different meanings, depending on the
current setting of the linear/log flag in SPPS.  This flag is
set in the final parameter in the SET call.
.sp
If the axis is linear: MJRx specifies the number of major
divisions of the X/Y axis and MNRx specifies the number of
minor divisions within each major division. In each case, the
value specifies the number of spaces between grid lines or
ticks rather than the number of lines or ticks. There is always
one more major division line or mark than the number of major
divisions specified by MJRx.  Similarly, there is always one
less minor division line or tick per major division than the
number of minor divisions per major division specified by
MNRx.
.sp
If the axis is logarithmic: Each major division point occurs at
a value 10**MJRx times the previous point. For example, if the
minimum X-axis value were 3., the maximum X-axis value 3000.
and MJRX 1, then the major division points would be 3., 30.,
300., and 3000. If MNRx.LE.10, there are nine minor divisions
within each major division.  For example, between 3. and 30.,
there would be minor division points at 6., 9., 12., . . . 27.
If MNRx.GT.10., minor divisions are omitted.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument description.
.SH ACCESS
To use PERIM load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_perim load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gacolr, gagetc, gageti, gagetr, gasetc, gaseti, gasetr, grid, gridal,
gridl, halfax, labmod, perim, periml, tick4, ticks, ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

