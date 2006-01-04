.TH SFLUSH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFLUSH - Flushes polylines, accumulated through calls to the routines
PLOTIF and PLOTIT, from the SPPS polyline buffer shared by those routines;
updates all open workstations; and flushes all system-level I/O buffers.
.SH STATUS
SFLUSH was called FLUSH in previous versions.  The name was changed
to avoid a naming conflict with a common system routine.
.SH SYNOPSIS
CALL SFLUSH
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sflush() 
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
(none)
.SH ACCESS
To use SFLUSH or c_sflush, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
ngpict, frame, plotif, spps, spps_params, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2006
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
