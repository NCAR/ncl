.TH ARSETR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARSETR - Provides a new real value for an Areas parameter.
.SH SYNOPSIS
CALL ARSETR (PNAM,RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_arsetr (char *pnam, float rval)
.SH DESCRIPTION 
.IP "PNAM" 12
(an input constant or variable of type CHARACTER) -
The name of the parameter that you want to set. The character string
can be of any length, but only the first two characters
of it will be examined.
.IP "RVAL" 12
(an input expression of type REAL) -
The real value you select for the parameter.  If the parameter is of type
REAL, it will be given the value RVAL; if is is of type INTEGER, it will be
given the value INT(RVAL).
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of Areas 
parameters. For a complete list of parameters available in this 
utility, see the areas_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cardb1,
arex01.
.SH ACCESS
To use ARSETR or c_arsetr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, aredam, argeti, argetr, argtai,
arinam, armvam, arpram, arscam, arseti, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
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
