'\" t
.TH SFSETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFSETI - Used to set the current integer value of a specified
internal parameter.
.SH SYNOPSIS
CALL SFSETI (CNP, IVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sfseti (char *cnp, int ivp)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is one of the 
character strings 'AN', 'CH', 'DO', 'SP', or 'TY' (or a 
longer string beginning with one of these strings), 
meaning "ANgle", "CHaracter", "DOts", "SPacing", and 
"TYpe", respectively.
.IP IVP
(an input expression of type INTEGER) is the desired new value 
of the parameter specified by CNP. This value will be used 
until the next call resetting it.
If the internal parameter is inherently of type REAL, the
value "REAL(IVP)" will be given to it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the 
FORTRAN argument descriptions.
.SH USAGE
This routine allows you to set the current value of Softfill
parameters. For a complete list of parameters available in
this utility, see the softfill_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant example:
agex12, cpex01, cpex02, cpex03, cpex04, cpex05, elblba, 
sfex01, sfex02, vvex01, tlblba, tsoftf, fsfsgfa.
.SH ACCESS
To use SFSETI or c_sfseti, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.  
.SH MESSAGES
See the softfill man page for a description of all Softfill 
error messages and/or informational messages.
.SH SEE ALSO
Online: 
softfill, softfill_params, sfgetc, sfgeti, sfgetp, sfgetr,
sfsetc, sfseti, sfsetp, sfsetr, sfsgfa, sfwrld, ncarg_cbind
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

