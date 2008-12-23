.\"
.\"	$Id: wmdrrg.m,v 1.13 2008-12-23 00:03:11 haley Exp $
.\"
.TH WMDRRG 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMDRRG - plots weather map regions (indicating "snow", "rain", etc.), or
solid regions indicating temperature zones.
.SH SYNOPSIS
CALL WMDRRG(N,X,Y,ITYPE,NC,XC,YC)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmdrrg(int n, float *x, float *y, char *itype, int nc, float *xc, float *yc)
.SH DESCRIPTION
.IP N 12
(Integer, Input) - The number of points supplied in the second and 
third arguments.
.IP X 12
(Real, Input) - The X coordinates  (specified in world coordinates) of 
N points that specify the control points of a spline curve that will be fitted
to represent the requested region.
.IP Y 12
(Real, Input) - The Y coordinates  (specified in world coordinates) of 
N points that specify the control points of a spline curve that will be fitted
to represent the requested region.
.IP ITYPE 12
(Character, Input) - Indicates the weather type to fill the region with, or
a solid color.
.RS
.IP "ITYPE can be one of:"
.sp
 'T' for thunderstorms
.sp
 'SH' for showers
.sp
 'R' for rain
.sp
 'F' for flurries
.sp
 'SN' for snow
.sp
 'I' for ice
.sp
 'INDEXnn' where the number "nn" denotes a color index for a solid fill.
.RE
.IP NC 12
(Integer, input) - If NC is greater than 2, then the region resulting from 
the spline fit to ((X(I),Y(I)),I=1,N) will be clipped against the region in 
((XC(I),YC(I)),I=1,NC). If no clipping is to be done, then set NC equal to 1.
.IP XC 12
(Real, Input) - The X coordinates  (specified in world coordinates) of
NC points that specify a clipping polygon for the region resulting from
the spline fit to ((X(I),Y(I)),I=1,N), if NC is greater than 2.
.IP YC 12
(Real, Input) - The Y coordinates  (specified in world coordinates) of
NC points that specify a clipping polygon for the region resulting from
the spline fit to ((X(I),Y(I)),I=1,N), if NC is greater than 2.
.SH USAGE
Set the values for the appropriate internal parameters before calling
WMDRRG to produce the desired region. The internal parameters that control
the appearance of regions are: COL and RHT.  As mentioned above, 
the points specified in the X and Y arrays serve as control points for
a spline curve that is fitted to the input polygon.  X(N) should equal
X(1) and Y(N) should equal Y(1).
.SH ACCESS
To use WMDRRG or c_wmdrrg, load the NCAR Graphics libraries ncarg, ncarg_gks, 
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
