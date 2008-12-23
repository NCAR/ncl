.\"
.\"	$Id: wmbarb.m,v 1.13 2008-12-23 00:03:11 haley Exp $
.\"
.TH WMBARB 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMBARB - plots wind barbs.
.SH SYNOPSIS
CALL WMBARB (X,Y,U,V)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmbarb(float x, float y, float u, float v)
.SH DESCRIPTION
.IP X 12
(Real, Input) - An X coordinate (specified in world coordinates) of a
point that specifies the position of the base of a wind barb.
.IP Y 12
(Real, Input) - A Y coordinate (specified in world coordinates) of a
point that specifies the position of the base of a wind barb.
.IP U 12
(Real, Input) - The X component of a wind vector.
.IP V 12
(Real, Input) - The Y component of a wind vector.
.SH USAGE
The angle at which the wind barb is drawn is determined by the vector 
(U,V) and the magnitude of that vector denotes the wind speed in knots.
Set the values for the appropriate internal parameters before calling
WMBARB to produce the wind barb.  The internal parameters that control
the appearance of wind barbs are: COL, WBA, WBD, WBF, WBS, WBT.
.SH ACCESS
To use WMBARB or c_wmbarb, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmlabs, wmsetc, wmseti, wmsetr, wmstnm, wmap_params
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
