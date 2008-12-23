.\"
.\"	$Id: wmlabc.m,v 1.13 2008-12-23 00:03:11 haley Exp $
.\"
.TH WMLABC 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMLABC - plots weather map city names and daily hi/lows.
.SH SYNOPSIS
CALL WMLABC (X, Y, CITY, TEMPS)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmlabc(float x, float y, char *city, char *temps)
.SH DESCRIPTION
.IP X 12
(Real, Input) - An X coordinate (specified in world coordinates) of a
point that specifies the center of the composite label that consists 
of the city name in the character variable CITY and the temperature 
ranges in the character variable TEMPS.  The city name is plotted above
the temperatures.
.IP Y 12
(Real, Input) - A Y coordinate (specified in world coordinates) of a
point that specifies the center of the composite label that consists 
of the city name in the character variable CITY and the temperature 
ranges in the character variable TEMPS.  The city name is plotted above
the temperatures.
.IP CITY 12
(Character, Input) - A character variable that contains the name of the
city being plotted.
.IP TEMPS 12
(Character, Input) - A character variable that contains the daily hi/low
label for the city named in the third argument.
.SH USAGE
Set the values for the appropriate internal parameters before calling
WMLABC to produce the desired label.  The internal parameters that control
the appearance of the labels are: CBC, CHT, CMG, RFC.  To mark the 
cities' position with a dot, see the documentation for WMLABS.
.SH ACCESS
To use WMLABC or c_wmlabc, load the NCAR Graphics libraries ncarg, ncarg_gks, 
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
