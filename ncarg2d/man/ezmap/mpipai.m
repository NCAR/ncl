.TH MPIPAI 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPIPAI - Given the area identifiers of two of the areas defined by whatever
database was last read by the EZMAPB routines MPLNAM, MPLNDM, MPLNDR, and
MPLNRI, this function has a non-zero value if and only if the first area is
a part of the second.
.SH SYNOPSIS
IPRT=MPIPAI(IAIN,IAIP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_mpipai (int iain, int iaip)
.SH DESCRIPTION 
.IP IAIN 12
is an input expression of type INTEGER, specifying the area identifier of a
particular area of interest.
.IP IAIP 12
is an input expression of type INTEGER, specifying the area identifier of a
second area of interest.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
The statement "IPRT=MPIPAI(IAIN,IAIP)" sets IPRT non-zero if and only if the
area having area identifier IAIN is a part of the area having area identifier
IAIP.  For example, if IMAD is the area identifier of the little island in
Lake Superior called "Lake Madeline", ICUS is the area identifier of the area
called 'Conterminous US', and IRUS is the area identifier of the area
called 'Russia', then MPIPAI(IMAD,ICUS) has a non-zero value, but
MPIPAI(IMAD,IRUS) does not.
.SH EXAMPLES
None.
.SH ACCESS
To use MPIPAI or c_mpipai, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
