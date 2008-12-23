.TH PLOTIT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PLOTIT - A line-drawing routine, described in terms of "pen moves".
Most calls to PLOTIT specify whether the "pen" should be up (not
drawing) or down (drawing) and then move it to a designated position
in the metacode coordinate system.  The polylines resulting from
the pen moves are buffered; some calls to PLOTIT just cause the SPPS
polyline buffer to be flushed.
.SH STATUS
Metacode units are no longer used in NCAR Graphics;
thus, PLOTIT is considered an obsolete routine.
.sp
The current recognized coordinate systems are GKS world coordinates,
GKS normalized device coordinates, NCAR Graphics fractional
coordinates, and NCAR Graphics user coordinates.  See the NCAR Graphics
User Document "NCAR Graphics Fundamentals, UNIX Version" for a description
of these coordinate systems.
.sp
PLOTIT continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use the routine
PLOTIF which uses fractional coordinates.
.sp
The following definition of the Metacode Coordinate System is included
for the purpose of interpreting and converting codes:
.sp
The metacode coordinates of a point are integers IMX and IMY between
0 and 32767 inclusive.  The area addressed is a square in a "metacode space"
that is usually mapped into a square subset of the addressable area of
the plotting device.  Metacode coordinates were used in calls to the
routine PLOTIT and are returned in calls to FL2INT.
.SH SYNOPSIS
CALL PLOTIT (IX,IY,IP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_plotit(int ix, int iy, int ip);
.SH DESCRIPTION 
.IP IX 12
(an input expression of type INTEGER) is the X metacode coordinate of the
point to which the plotter pen is to be moved.
.IP IY 12
(an input expression of type INTEGER) is the Y metacode coordinate of the
point to which the plotter pen is to be moved.
.IP IP 12
(an input expression of type INTEGER) determines whether the
movement of the plotter pen to the point (IX,IY) will occur with the pen up
(IP = 0), or with the pen down (IP = 1).  If IP = 2, no pen move occurs,
but the SPPS polyline buffer is flushed.  For historical reasons, a
"CALL PLOTIT (0,0,0)" will also flush the buffer.
.sp
The size of the SPPS polyline buffer can be changed by calling
the parameter-setting routine SETUSV to set the parameter 'PB'.  The legal
range of 'PB' is between 2 and 50.  For example, to set 'PB' to 2, use
"CALL SETUSV ('PB',2)".
.sp
The SPPS polyline buffer is provided to increase drawing efficiency.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH ACCESS
To use PLOTIT or c_plotit, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gpl, fl2int, plotif, setusv, getusv, sflush, frstpt, vector, line, curve,
spps, spps_converters
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
