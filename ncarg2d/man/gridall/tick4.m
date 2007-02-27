.TH TICK4 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TICK4 - 
Allows for program control of tick mark length and
direction.
.SH SYNOPSIS
CALL TICK4 (LMJX, LMNX, LMJY, LMNY)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tick4 (int lmjx, int lmnx, int lmjy, int lmny)
.SH DESCRIPTION 
All arguments are in plotter address units (PAUs). By
default, one PAU is 1/1023rd of the width of the plotter
frame.
.sp
Note: A call to the SPPS routine SETI will change the
definition of a PAU. Such calls are no longer recommended,
but the possibility still exists.
.IP "LMJX and LMNX" 12 
(input expressions of type INTEGER) are the
lengths, in plotter address units, of major and minor ticks
on the X axis. The default values are 12 and 8.
.IP "LMJY and LMNY" 12
(input expressions of type INTEGER) are the
lengths, in plotter address units, of major and minor ticks
on the Y axis. The default values are 12 and 8.
.IP "" 0
By default, tick marks point inward. Negative values of
LMJX, LMNX, LMJY, and LMNY may be used to create outward-pointing
tick marks.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Gridall parameters.  For a complete list of parameters available
in this utility, see the gridall_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
tgrida.
.SH ACCESS
To use TICK4 or c_tick4, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gridall,
gridall_params,
gacolr,
gagetc,
gageti,
gagetr,
gasetc,
gaseti,
gasetr,
grid,
gridal,
gridl,
halfax,
labmod,
perim,
periml,
ticks,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
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
