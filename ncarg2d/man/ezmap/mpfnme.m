.TH MPFNME 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPFNME - Given the area identifier of one of the areas defined by whatever
database was last read by one of the EZMAPB routines MPLNAM, MPLNDM, MPLNDR,
and MPLNRI, this function returns the full name of the area, including the
prepended names of all containing (parent) areas, up to and including a
specified level.
.SH SYNOPSIS
CHARACTER*128 MPFNME,FNME
.sp
FNME=MPFNME(IAIN,ILVL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
char *c_mpfnme (int iain, int ilvl)
.SH DESCRIPTION 
.IP IAIN 12
is an input expression of type INTEGER, specifying the area identifier of a
particular area of interest.
.IP ILVL 12
is an input expression of type INTEGER, specifying the highest level of
containing (parent) name to be included in the returned full name.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
Given the declaraion "CHARACTER*128 MPFNME,FNME", the executable statement
"FNME=MPFNME(IAIN,ILVL)" will retrieve in FNME the full name of the area with
area identifier IAIN, including the prepended names of containing (parent)
areas, up to the level ILVL.  For example, if IMAD is the area identifier of
the little island in Lake Superior called "Lake Madeline", a level-4 area
whose name in the database is actually "Madeline Island (Lake Superior)" and
whose parent (also at level 4) is an area called "Wisconsin", then
MPFNME(IMAD,4) = 'Wisconsin - Madeline Island (Lake Superior)'.  Note the use
of a dash surrounded by blanks as a separator.
.SH EXAMPLES
Use the ncargex command to see the following relevant example: mpex12.
.SH ACCESS
To use MPFNME or c_mpfnme, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
