.\"
.\"	$Id: dashline.m,v 1.1 1993-04-06 19:38:37 haley Exp $
.\"
.TH DASHLINE 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
DASHLINE - draws and smoothes dashed lines.
.SH SYNOPSIS
CALL DASHDC - choose a dash pattern with labels 
.br
CALL DASHDB - chooses a dash pattern without labels
.br
CALL CURVED - draws a curve through points.
.br
CALL FRSTD - put pen down
.br
CALL VECTD - draw a line segment
.br
CALL LINED - draw a straight line between two points.
.br
CALL LASTD - used to flush buffers and finish drawing a line
after calling FRSTD, VECTD, or LINED.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_dashdc
.br
c_dashdb
.br
c_curved
.br
c_frstd
.br
c_vectd
.br
c_lined
.br
c_lastd
.SH ACCESS 
To use DASHLINE routines load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_loc, preferably in that order.  To use the
DASHLINE C-bindings, load the NCAR Graphics libraries ncargC, ncarg_gksC,
ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
.sp
To get the smooth version of CURVED, load libdashsmth.o, or add
the -dashsmooth option to your ncargf77 command.  To get the
super version of CURVED, load libdashsupr.o, or add the
-dashsuper option to your ncargf77 command.
.SH SEE ALSO
Online:
curved, dashline, dashdb, dashdc, frstd, lined, vectd, ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

