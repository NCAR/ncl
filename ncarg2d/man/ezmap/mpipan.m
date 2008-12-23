.TH MPIPAN 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPIPAN - Given the area identifier of one of the areas defined by whatever
database was last read by the EZMAPB routines MPLNAM, MPLNDM, MPLNDR, and
MPLNRI and a name, this function has a non-zero value if and only if the
area with the specified area identifier is part of some area with the
specified name.
.SH SYNOPSIS
IPRT=MPIPAN(IAIN,ANME)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_mpipan (int iain, char *anme)
.SH DESCRIPTION 
.IP IAIN 12
is an input expression of type INTEGER, specifying the area identifier of a
particular area of interest.
.IP ANME 12
is an input expression of type CHARACTER, specifying a name of interest.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
The statement "IPRT=MPIPAN(IAIN,ANME)" sets IPRT non-zero if and only if the
area having area identifier IAIN is a part of some area having the name ANME.
For example, if IMAD is the area identifier of the little island in Lake
Superior called "Lake Madeline", then MPIPAN(IMAD,'Conterminous US') has a
non-zero value, but MPIPAN(IMAD,'Russia') does not.
.SH EXAMPLES
None.
.SH ACCESS
To use MPIPAN or c_mpipan, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
