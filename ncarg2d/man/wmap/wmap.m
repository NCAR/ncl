.TH Wmap 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Wmap - A Package for Producing Daily Weather Maps and Plotting 
Station Model Data
.SH SYNOPSIS
WMBARB -
Plots wind barbs.
.sp
WMDFLT - 
Returns all values of all internal parameters to their defaults.
.sp
WMDRFT -
Draws weather fronts.
.sp
WMDRRG -
Draws weather regions (indicating "snow", "rain", etc.) or regions of
solid color indicating temperature regions.
.sp
WMGETC -
Retrieves the character value of a specified internal parameter.
.sp
WMGETI -
Retrieves the integer value of a specified internal parameter.
.sp
WMGETR -
Retrieves the real value of a specified internal parameter.
.sp
WMLABC -
Plots city names and daily hi/lows.
.sp
WMLABS -
Plots special symbols and daily weather icons.
.sp
WMLABT -
Plots regional temperature labels.
.sp
WMLABW -
Plots regional weather labels.
.sp
WMLGND -
Plots weather map legends.
.sp
WMSETC -
Gives a new character value to a specified internal parameter.
.sp
WMSETI -
Gives a new integer value to a specified internal parameter.
.sp
WMSETR -
Gives a new real value to a specified internal parameter.
.sp
WMSTNM -
Plots station model data.
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_wmbarb
.br
c_wmdflt
.br
c_wmdrft
.br
c_wmdrrg
.br
c_wmgetc
.br
c_wmgeti
.br
c_wmgetr
.br
c_wmlabc
.br
c_wmlabs
.br
c_wmlabt
.br
c_wmlabw
.br
c_wmlgnd
.br
c_wmsetc
.br
c_wmseti
.br
c_wmsetr
.br
c_wmstnm
.SH ACCESS 
To use Wmap Fortran or C entries, load the NCAR Graphics libraries
ncarg, ncarg_gks, and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
wmap_params,
wmbarb, 
wmdflt, 
wmdrft, 
wmdrrg, 
wmgetc, 
wmgeti, 
wmgetr, 
wmlabc, 
wmlabs,
wmlabt, 
wmlabw,
wmlgnd, 
wmsetc, 
wmseti, 
wmsetr, 
wmstnm,
ncarg_cbind.
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
