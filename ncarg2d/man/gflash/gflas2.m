.TH GFLAS2 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GFLAS2 - Terminates putting plotting
instructions to disk and resumes putting plotting instructions
to the the output stream. A call to GFLAS2 can only be made after
a previous call to GFLAS1.
.SH SYNOPSIS
CALL GFLAS2 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gflas2() 
.SH DESCRIPTION
None.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:
ccpmovi,
tgflas,
fgke02.
.SH ACCESS
To use GFLAS2 or c_gflas2, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gflash,
gflas1,
gflas3,
gflas4,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
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
