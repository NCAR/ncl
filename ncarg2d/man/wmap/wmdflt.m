.\"
.\"	$Id: wmdflt.m,v 1.9 2005-01-04 15:42:33 haley Exp $
.\"
.TH WMDFLT 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMDFLT - returns all values of internal parameters in the Wmap package to their default values.
.SH SYNOPSIS
CALL WMDFLT ()
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmdflt()
.SH USAGE
All values of all internal parameters are returned to their default values.
See the man page for wmap_params for default values.
.SH ACCESS
To use WMDFLT or c_wmdflt, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmlabs, wmsetc, wmseti, wmsetr, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
.SH COPYRIGHT
Copyright (C) 1987-2005
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
