.TH MPISCI 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPISCI - Given the area identifier of one of the areas defined by whatever
database was last read by one of the EZMAPB routines MPLNAM, MPLNDM, MPLNDR,
and MPLNRI, this function returns a suggested color index for that area.
.SH SYNOPSIS
ISCI=MPISCI(IAIN)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_mpisci (int iain)
.SH DESCRIPTION 
.IP IAIN 12
is an input expression of type INTEGER, specifying the area identifier of a
particular area of interest.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
The statement "ISCI=MPISCI(IAIN)" retrieves in ISCI the suggested color index
for the area with area identifier IAIN.  For example, if IMAD is the area
identifier of the little island in Lake Superior called "Lake Madeline", then
MPISCI(IMAD) has the value 6.  The suggested color indices are defined in such
a way as to ensure that areas that touch each other have different color
indices.
.sp
To get a suggested color index for the area, at a specified level, that
contains the area with a specified area identifier, use MPISCI in combination
with MPIOAR (which see).  For example, MPISCI(MPIOAR(IMAD,4)) has the value
6, which is the color index for "Wisconsin" or any portion thereof, and
MPISCI(MPIOAR(IMAD,2)) = 5, which is the color index for "North America".
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: mpex11, mpex12.
.SH ACCESS
To use MPISCI or c_mpisci, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
