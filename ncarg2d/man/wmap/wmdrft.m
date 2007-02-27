.\"
.\"	$Id: wmdrft.m,v 1.11 2007-02-27 18:20:38 haley Exp $
.\"
.TH WMDRFT 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMDRFT - plots weather map front lines.
.SH SYNOPSIS
CALL WMDRFT (N, X, Y)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmdrft(int n, float *x, float *y)
.SH DESCRIPTION
.IP N 12
(Integer, Input) - The number of points 
supplied in the second and third arguments.
.IP X 12
(Real, Input) - The X coordinates  (specified in world coordinates) of 
N points that specify the control points of a spline curve that will be fitted
to represent the requested weather front.
.IP Y 12
(Real, Input) - The Y coordinates  (specified in world coordinates) of 
N points that specify the control points of a spline curve that will be fitted
to represent the requested weather front.
.SH USAGE
Set the values for the appropriate internal parameters before calling
WMDRFT to produce the desired weather front.  The internal parameters that
control the appearance of weather fronts are: ALO, ARC, BEG, BET, CFC, CS1,
CS2, COL, DWD, END, FRO, LIN, NBZ, NMS, PAI, REV, SL1, SL2, SLF, STY, SWI,
T1C, T2C, WFC, and WTY.  The possible fronts are:
warm, warm aloft, cold, cold aloft, stationary, stationary aloft, occluded, 
convergence line, instability line, intertropical.  As mentioned above, 
the points specified in the X and Y arrays serve as control points for
a spline curve that is fitted between the first and last point.  The spline
passes through the given points.  If the desired front line is reasonably 
smooth, then only a few input points need be specified.
.SH ACCESS
To use WMDRFT or c_wmdrft, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmsetc, wmseti, wmsetr, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
.SH COPYRIGHT
Copyright (C) 1987-2007
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
