.TH GACOLR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GACOLR - 
Sets the values of internal parameters determining the
color of various parts of the background.
.SH SYNOPSIS
CALL GACOLR (KAXS,KLBL,KMJT,KMNT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gacolr (int kaxs, int klbl, int kmjt, int kmnt)
.SH DESCRIPTION 
.IP "KAXS, KLBL, KMJT, and KMNT" 12
(input expressions of type
INTEGER) are new values for the internal parameters \'CAX\',
\'CLB\', \'CMJ\', and \'CMN\', specifying the color indices to be
used for the axes, the labels, the major ticks/grid lines,
and the minor ticks/grid lines, respectively. Values less
than zero imply that no call is to be done to set the color
before drawing items of the associated type. The default
value of each of the four internal parameters is -1.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Gridall parameters.  For a complete list of parameters available
in this utility, see the gridall_params man page.
.SH ACCESS
To use GACOLR or c_gacolr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
gridall,
gridall_params,
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
tick4,
ticks,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
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
