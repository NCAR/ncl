.TH MAPITA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPITA - Adds to an area map the projections of lines defined by a series
of lat/lon coordinates.
.SH SYNOPSIS
CALL MAPITA (RLAT,RLON,IFST,IAMA,IGID,IAIL,IAIR)
.sp
CALL MAPIQA (IAMA,IGID,IAIL,IAIR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapita (float rlat, float rlon, int ifst, 
.br
int *iama, int igid, int iail, int iair)
.sp
void c_mapiqa (int *iama, int igid, int iail, int iair)
.SH DESCRIPTION 
.IP "RLAT and RLON" 12 
(input expressions, of type REAL) specify the latitude and
longitude of a point to which the "pen" is to be moved. Both are given in
degrees. RLAT must be between -90. and +90., inclusive; RLON must be
between -540. and +540., inclusive.
.IP "IFST" 12 
(an input expression, of type INTEGER) is 0 to do a "pen-up" move, 1
to do a "pen-down" move only if the distance from the last point to the
new point is greater than 'MV' plotter units, and 2 or greater to do
a "pen-down" move regardless of the distance from the last point to the
new one.
.IP "IAMA" 12 
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
The routines MAPITA and MAPIQA may be used to add lines defined by a set
of user-specified latitudes and longitudes to an area map; they attempt
to omit non-visible portions of lines and to handle "cross-over", in the
same way that MAPIT does.
.sp
MAPITA is called like the Ezmap routine MAPIT, but it has
some additional arguments:
.sp
.RS 4
CALL MAPITA (RLAT,RLON,IFST,IAMA,IGID,IAIL,IAIR)
.RE
.sp
Additional arguments are the area map array IAMA, a group identifier
IGID, and left and right area identifiers IAIL and IAIR. MAPIQA is called
like the Ezmap routine MAPIQ, to terminate a series of calls to MAPITA
and to flush the buffers; it has the same additional arguments:
.sp
.RS 4
CALL MAPIQA (IAMA,IGID,IAIL,IAIR)
.RE
.sp
The additional arguments are passed by MAPITA and MAPIQA to the routine
AREDAM, in the package named AREAS. 
.sp
For more information, see the man page for the routine MAPIT.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cmpita,
cpex08.
.SH ACCESS
To use MAPITA, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order. To use c_mapita, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and 
ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ezmap,
ezmap_params,
mapaci,
mapbla,
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
mapiqm,
mapit,
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
mpgetc,
mpgeti,
mpgetl,
mpgetr,
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
