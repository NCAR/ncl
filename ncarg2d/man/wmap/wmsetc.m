.TH WMSETC 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
WMSETC - Sets the value of an internal parameter of type CHARACTER.
.SH SYNOPSIS
CALL WMSETC (PNAM,CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_wmsetc (char *pnam, char *cval)
.SH DESCRIPTION 
.IP PNAM 12
(an input constant or variable of type CHARACTER) specifies the name of the
parameter to be set. The name must appear as the first three
characters of the string.
.IP CVAL 12
(an input expression of type CHARACTER)
is the value to be assigned to the
internal parameter specified by PNAM.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
Wmap parameters.  For a complete list of parameters available
in this utility, see the wmap_params man page.
.SH ACCESS
To use WMSETC or c_wmsetc, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmlabs, wmsetr, wmseti, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
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
