.TH PLOTIF 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PLOTIF - Moves the plotter pen to a designated position in fractional
coordinates, or causes a pen-move buffer flush.
.SH SYNOPSIS
CALL PLOTIF (FX,FY,IP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_plotif (float fx, float fy, int ip)
.SH DESCRIPTION 
.IP FX 12
(an input coordinate of type REAL) is the X fractional coordinate of the
point to which the plotter pen is to be moved.
.IP FY 12
(an input coordinate of type REAL) is the Y fractional coordinate of the
point to which the plotter pen is to be moved.
.IP IP 12
(an input parameter of type INTEGER) which determines whether the
movement of the plotter pen to point FX,FY will occur with the pen up
(IP = 0), or with the pen down (IP = 1).  This parameter (IP = 2) can
also be used to flush the pen-move buffer.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
"CALL PLOTIF (0,0,0)" will also flush the buffer.
.sp
The size of the pen-move buffer can be changed by a call to
the parameter setting routine SETUSV, with parameter PB.  The legal
range of parameter PB is between 2 and 50.  For example, to set
PB to 2, CALL SETUSV("PB",2).
.sp
NOTE!
.sp
The PLOTIF buffer is provided to increase drawing efficiency.
However, there are drawing side affects that can occur because
the polyline segments get buffered.  For example, if one wanted
to change the color of a line at a certain point, one would have
to remember to call SFLUSH to flush the existing buffer before
calling GSPLCI to change the polyline color index.  This applies
to the changing of any polyline attribute.  One must remember to
flush the PLOTIF buffer.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
arex01, cbex01, cmppos, coex03, cpexcc, epltch.
.SH ACCESS
To use PLOTIF, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_plotif, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gpl, plotit, setusv, getusv, sflush, frstpt, vector, line, curve,
spps, spps_params, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
