.TH EPRIN 3NCARG "March 1994" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EPRIN - Prints the current error message.
.SH SYNOPSIS
CALL EPRIN
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_eprin()
.SH DESCRIPTION 
If SETER's internal error flag is non-zero, the FORTRAN statement "CALL EPRIN"
prints the remembered error message associated with the non-zero value.
Otherwise, EPRIN does nothing.
.sp
EPRIN has no arguments.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
tseter,
arex02.
.SH ACCESS
To use EPRIN or c_eprin, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
entsr, errof, error_handling, fdum, icfell, icloem, nerro, retsr, semess, seter,
ncarg_cbind
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
