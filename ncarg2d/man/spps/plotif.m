.TH PLOTIF 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
PLOTIF - A line-drawing routine, described in terms of "pen moves".
Most calls to PLOTIF specify whether the "pen" should be up (not
drawing) or down (drawing) and then move it to a designated position
in the fractional coordinate system.  The polylines resulting from
the pen moves are buffered; some calls to PLOTIF just cause the SPPS
polyline buffer to be flushed.
.SH SYNOPSIS
CALL PLOTIF (FX,FY,IP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_plotif (float fx, float fy, int ip)
.SH DESCRIPTION 
.IP FX 12
(an input expression of type REAL) is the X fractional coordinate of the
point to which the plotter pen is to be moved.  If IP = 2, this argument
is ignored.
.IP FY 12
(an input expression of type REAL) is the Y fractional coordinate of the
point to which the plotter pen is to be moved.  If IP = 2, this argument
is ignored.
.IP IP 12
(an input expression of type INTEGER) determines whether the
movement of the plotter pen to the point (FX,FY) will occur with the pen up
(IP = 0), or with the pen down (IP = 1).  If IP = 2, no pen move occurs,
but the SPPS polyline buffer is flushed.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
"CALL PLOTIF (FX,FY,0)" moves the pen to a new starting position.
.sp
"CALL PLOTIF (FX,FY,1)" moves the pen to a new position, drawing a
straight line segment from the previous position to the new one.
.sp
"CALL PLOTIF (0.,0.,2)" flushes the SPPS polyline buffer.
.sp
The size of the SPPS polyline buffer can be changed by calling
the parameter-setting routine SETUSV to set the parameter 'PB'.  The legal
range of 'PB' is between 2 and 50.  For example, to set 'PB' to 2, use
"CALL SETUSV ('PB',2)".
.sp
NOTE!
.sp
The SPPS polyline buffer is provided to increase drawing efficiency.
However, there are side effects that can occur because
the polyline segments get buffered.  For example, if one wanted
to change the color of a line at a certain point, one would have
to remember to flush the buffer before calling GSPLCI to
change the polyline color index.  This applies to the changing of
any polyline attribute.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
arex01, cbex01, cmppos, coex03, cpexcc, epltch.
.SH ACCESS
To use PLOTIF or c_plotif, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
gpl, plotit, setusv, getusv, sflush, frstpt, vector, line, curve,
spps, spps_params, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2003
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
