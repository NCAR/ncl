.TH MAPVEC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPVEC -
Draws lines on a map - used in conjunction with MAPFST.
.SH SYNOPSIS
CALL MAPVEC (RLAT,RLON)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapvec (float rlat, float rlon)
.SH DESCRIPTION 
.IP "RLAT and RLON" 12
(input expressions, of type REAL) specify the latitude and
longitude of a point to which the "pen" is to be moved. Both are given in
degrees. RLAT must be between -90. and +90., inclusive; RLON must be
between -540. and +540., inclusive.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE@@@
The statement:
.sp
.RS 5
CALL MAPVEC (RLAT, RLON)
.RE 
.sp
is equivalent to
.RS 5
.sp
CALL MAPIT (RLAT, RLON, 1)
.RE
.sp
It moves the "pen" (in the "down" position) to the next point in a series
of points.
See the man page for MAPIT.
.SH ACCESS
To use MAPVEC, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_mapvec, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and 
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
mapita,
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
