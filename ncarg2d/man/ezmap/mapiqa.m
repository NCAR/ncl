.TH MAPIQA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPIQA - Terminates a string of calls to MAPITA.
.SH SYNOPSIS
CALL MAPIQA (IAMA,IGID,IAIL,IAIR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapiqa (int *iama, int igid, int iail, int iair)
.sp
void c_mapita (float rlat, float rlon, int ifst, 
.br
int *iama, int igid, int iail, 
int iair)
.SH DESCRIPTION 
.IP IAMA 12 
(an input/output array of type INTEGER, dimensioned as specified in
a call to the AREAS initialization routine ARINAM) is the area map to
which lines in MAPITA's buffer are to be added.
.IP IGID 12 
(an input expression of type INTEGER) is the group identifier to be
passed to the AREAS routine AREDAM when the lines in MAPITA's buffer are
added to the area map in IAMA.
.IP IAIL 12 
(an input expression of type INTEGER) is the left area identifier to
be passed to the AREAS routine AREDAM when the lines in MAPITA's buffer
are added to the area map in IAMA.
.IP IAIR 12 
(an input expression of type INTEGER) is the right area identifier
to be passed to the AREAS routine AREDAM when the lines in MAPITA's
buffer are added to the area map in IAMA.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
You must call MAPITA once for each point along the line.  After
your last call to MAPITA for a given line, you must call MAPIQA
to signal the end of the line.
.sp
For more information, see the man pages for the routines MAPIT and MAPITA.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cmpita,
cpex08.
.SH ACCESS
To use MAPIQA or c_mapiqa, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
