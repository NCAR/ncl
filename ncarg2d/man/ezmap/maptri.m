.TH MAPTRI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPTRI - Performs inverse transformations. 
.SH SYNOPSIS
CALL MAPTRI (UVAL,VVAL,RLAT,RLON)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maptri (float uval, float vval, float *rlat, 
.br
float *rlon)
.SH DESCRIPTION 
.IP "UVAL and VVAL" 12
(input expressions, of type REAL) define the point
(UVAL,VVAL) that is the projection in the u/v plane of the point whose
latitude and longitude are desired. The units of UVAL and VVAL depend on
the projection.
.IP "RLAT and RLON" 12
(output variables, of type REAL) are the latitude and
longitude, respectively, of the point, in degrees. RLAT will be
between -90. and +90., inclusive; RLON will be between -180. and +180.,
inclusive.
.sp
If the point (UVAL,VVAL) is not the projection of some point of the globe
or is outside the boundary of the map, RLAT and RLON are returned equal
to 1.E12.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpmpxy,
mpex10.
.SH ACCESS
To use MAPTRI or c_maptri, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
