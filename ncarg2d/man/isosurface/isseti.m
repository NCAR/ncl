'\" t
.TH ISSETI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ISSETI - Resets the current values of internal parameters
of type INTEGER.
.SH SYNOPSIS
CALL ISSETI (CNP,IVP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_isseti (char *cnp, int ivp)
.SH DESCRIPTION 
.IP CNP 12
(an input expression of type CHARACTER) is the name of
an internal parameter. Only the first two characters of the
name are meaningful, but a longer character string may be
used to more clearly document what internal parameter is
being set.
.IP IVP 12
(an input expression of type INTEGER) 
is the desired new value of the
parameter specified by CNP. This value will be used until
the next call resetting it.
If the internal parameter is inherently of type REAL, the
value "REAL(IVP)" will be given to it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Isosurface parameters.  For a complete list of parameters available
in this utility, see the isosurface_params man page.
.SH ACCESS
To use ISSETI or c_isseti, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the isosurface man page for a description of all Isosurface error
messages and/or informational messages.
.SH SEE ALSO
Online:
isosurface, isosurface_params, ezisos, 
isgeti, isgetr, isosrf, issetr, pwrzi, 
ncarg_cbind
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
