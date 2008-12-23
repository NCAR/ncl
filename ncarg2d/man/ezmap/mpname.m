.TH MPNAME 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPNAME - Given the area identifier of one of the areas defined by whatever
database was last read by one of the EZMAPB routines MPLNAM, MPLNDM, MPLNDR,
and MPLNRI, this function returns the name of the area.
.SH SYNOPSIS
CHARACTER*64 MPNAME,NAME
.sp
NAME=MPNAME(IAIN)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
char *c_mpname (int iain)
.SH DESCRIPTION 
.IP IAIN 12
is an input expression of type INTEGER, specifying the area identifier of a
particular area of interest.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
Given the declaration "CHARACTER*64 MPNAME,NAME", the executable statement
"NAME=MPNAME(IAIN)" will retrieve in NAME the name of the area with area
identifier IAIN.  For example, if IMAD is the area identifier of the little
island in Lake Superior called "Lake Madeline", then MPNAME(IMAD) = 'Madeline
Island (Lake Superior)'.
.sp
To get the name of the area, at a specified level, that contains an area
with a specified area identifier, use MPNAME in combination with MPIOAR.  For
example, MPNAME(MPIOAR(IMAD,3)) = 'Conterminous US', and MPNAME(MPIOAR(IMAD,2))
= 'North America'.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: mpex11, mpex12.
.SH ACCESS
To use MPNAME or c_mpname, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
mpisci,
mplnam,
mplndm,
mplndr,
mplnri,
mprset,
mpsetc,
mpseti,
mpsetl,
mpsetr,
supmap,
supcon,
ncarg_cbind
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
