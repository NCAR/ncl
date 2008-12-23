.\"
.\"	$Id: wmdrft.m,v 1.13 2008-12-23 00:03:11 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
