.TH MAPRS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPRS - Re-calls SET. 
.SH SYNOPSIS
CALL MAPRS 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maprs()
.SH DESCRIPTION
MAPRS has no arguments.
.SH USAGE
This routine allows you to have the SPPS routine SET re-called with the
same arguments that MAPINT would use, but executes a little faster than
MAPINT.  This is useful when you are about to call one of the
routines MAPBLA, MAPDRW, MAPFST, MAPGRD, MAPGRM, MAPIQ, MAPIQA, MAPIQM,
MAPIT, MAPITA, MAPITM, MAPLBL, MAPLOT, or MAPVEC (all of which depend on
the proper SET call's having been done), and there has been some intervening,
inappropriate, call to SET.
.SH EXAMPLES
No example is available for MAPRS.
.SH ACCESS
To use MAPRS or c_maprs, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
maplbl,
maplmb,
maplot,
mappos,
maproj,
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
