.TH MPIOSA 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPIOSA - Given the area identifier of one of the areas defined by whatever
database was last read by one of the EZMAPB routines MPLNAM, MPLNDM, MPLNDR,
and MPLNRI, this function returns the area identifier of the smallest area,
at a specified level, that contains it.
.SH SYNOPSIS
IOSA=MPIOSA(IAIN,ILVL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_mpiosa (int iaid, int ilvl)
.SH DESCRIPTION 
.IP IAIN 12
is an input expression of type INTEGER, specifying the area identifier of a
particular area of interest.
.IP ILVL 12
is an input expression of type INTEGER, specifying the level of the containing
area to be found.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
The statement "IOSA=MPIOSA(IAIN,ILVL)" retrieves in IOSA the area identifier
of that area, at level ILVL, that contains the area with area identifier IAIN.
For example, if IMAD is the area identifier of the little island in Lake
Superior called "Lake Madeline", then MPIOSA(IMAD,3) is the area identifier
of the area called "Conterminous US".
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: mpex11, mpex12.
.SH ACCESS
To use MPIOSA or c_mpiosa, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
