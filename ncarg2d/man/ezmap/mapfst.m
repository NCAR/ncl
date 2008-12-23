.TH MAPFST 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nb
.SH NAME
MAPFST - 
Draws lines on a map - used in conjunction with MAPVEC.
.SH SYNOPSIS
CALL MAPFST (RLAT,RLON)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapfst (float rlat, float rlon)
.SH DESCRIPTION 
.IP "RLAT and RLON" 12 
(input expressions, of type REAL) specify the latitude and
longitude of a point to which the "pen" is to be moved. Both are given in
degrees. RLAT must be between -90. and +90., inclusive; RLON must be
between -540. and +540., inclusive.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The statement:
.RS 5
.sp
CALL MAPFST (RLAT, RLON)
.sp
.RE 
is equivalent to 
.RS 5
.sp
CALL MAPIT (RLAT, RLON, 0)
.RE
.sp
It moves the "pen" (in the "up" position) to the first point in a series
of points.
See the man page for MAPIT.
.SH ACCESS
To use MAPFST or c_mapfst, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
