.TH MAPLBL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPLBL - Labels the map.
.SH SYNOPSIS
CALL MAPLBL 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maplbl()
.SH DESCRIPTION
MAPLBL has no arguments.
.SH USAGE 
If the parameter 'LA' is set appropriately, MAPLBL labels the
international date line (ID), the equator (EQ), the Greenwich
Meridian (GM), and the poles (NP and SP), and if the parameter
\&'PE' is set appropriately, it draws the perimeter of the map.
If EZMAP needs initialization or if there is an uncleared NCAR Graphics
error, MAPLBL does nothing.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cmpclr,
cmpel,
cmpfil,
cmpgrp,
cmpita,
cmpitm,
cmplab,
cmplbl,
cmplot,
cmpmsk,
cmpou,
cmptit,
eezmpa,
tezmpa,
ffex00.
.SH ACCESS
To use MAPLBL or c_maplbl, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
ezmap,
ezmap_params,
mapaci,
mapbla,
mapblm,
mapdrw,
mapeod,
mapfst,
mapgci,
mapgrd,
mapgrm,
mapgtc,
mapgti,
mapgtl,
mapgtr,
mapint,
mapiq,
mapiqa,
mapiqd,
mapiqm,
mapit,
mapita,
mapitd,
mapitm,
maplmb,
maplot,
mappos,
maproj,
maprs,
maprst,
mapsav,
mapset,
mapstc,
mapsti,
mapstl,
mapstr,
maptra,
maptri,
maptrn,
mapusr,
mapvec,
mpchln,
mpfnme,
mpgetc,
mpgeti,
mpgetl,
mpgetr,
mpglty,
mpiaty,
mpifnb,
mpilnb,
mpiola,
mpiosa,
mpipai,
mpipan,
mpipar,
mpisci,
mplnam,
mplndm,
mplndr,
mplnri,
mpname,
mprset,
mpsetc,
mpseti,
mpsetl,
mpsetr,
supmap,
supcon,
ncarg_cbind
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
