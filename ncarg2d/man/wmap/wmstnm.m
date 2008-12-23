.\"
.\"	$Id: wmstnm.m,v 1.13 2008-12-23 00:03:11 haley Exp $
.\"
.TH WMSTNM 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMSTNM - plots station model data as per WMO guidelines.
.SH SYNOPSIS
CALL WMSTNM (X, Y, IMDAT)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmstnm(float x, float y, char *imdat)
.SH DESCRIPTION
.IP X 12
(Real, Input) - An X coordinate (specified in world coordinates) of a
point that specifies the position of the
base of the wind barb shaft in the station model
display.
.IP Y 12
(Real, Input) - A Y coordinate (specified in world coordinates) of a
point that specifies the position of the
base of the wind barb shaft in the station model
display.
.IP IMDAT 12
(Character, Input) - A CHARACTER*5 array of dimension 10 encoded as per 
the WMO/NOAA guidelines.
.SH USAGE
Set the values for the appropriate internal parameters before calling
WMSTNM to produce station model data plots.  The internal parameters 
that control the appearance of the station model data plots are:  WBC and WBL.
The appearance of the wind barb part of the station model data is controlled
by the internal parameters applying to wind barbs.  See the documentation
for WMBARB for a description of wind barbs.
.SH ACCESS
To use WMSTNM or c_wmstnm, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmlabs, wmsetc, wmseti, wmsetr, wmap_params
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
