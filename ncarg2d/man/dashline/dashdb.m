.TH DASHDB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DASHDB - Defines a dash pattern without labels.
.SH SYNOPSIS
CALL DASHDB (IPAT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dashdb (int *ipat)
.SH DESCRIPTION 
.IP IPAT 12
(an input expression of type INTEGER) is
a 16-bit dash pattern (1=solid, 0=blank); e.g., the binary
number 1111000011110000, which is a decimal 61680,
will give dashes of medium length.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
carline,
colcon,
cmpfil,
cmpitm,
cmplab,
cmpmsk,
cmptit,
cmpusr,
mpex08,
tdashc,
tdashl,
tdashp,
tdashs,
fdldashd.
.SH USAGE
DASHDB may be called to define a dash pattern for any of the four
versions of Dashline.
.sp
A dash pattern defined by a call to DASHDB will supersede one defined
by an earlier call to DASHDB or DASHDC.
.SH ACCESS
To use DASHDB or c_dashdb, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
dashline, dashline_params,
curved, dashdc, frstd, lastd, lined, reset, vectd, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
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
