.TH MAPTRN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPTRN - Projects points.
.sp
SUPCON is an alternate name for the routine MAPTRN.
.SH SYNOPSIS
CALL MAPTRN (RLAT, RLON, UVAL, VVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maptrn (float rlat, float rlon, float *uval, 
.br
float *vval)
.SH DESCRIPTION 
.IP "RLAT and RLON" 12 
(input expressions, of type REAL) are the latitude and
longitude, respectively, of a point on the globe. RLAT must be
between -90. and +90., inclusive; RLON must be between -540. and +540.,
inclusive.
.IP "UVAL and VVAL" 12 
(output variables, of type REAL) define the point
(UVAL,VVAL) that is the projection in the u/v plane of (RLAT,RLON). The
units of UVAL and VVAL depend on the projection.
.sp
If the point is not projectable, UVAL is returned equal to 1.E12.
If the point is projectable, but outside the boundary of the map
(as defined by the last call to MAPSET and by the value of the
parameter 'EL'), the routine MAPTRN, unlike
MAPTRA, still returns its u and v coordinates.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cpex03,
mpex09.
.SH ACCESS
To use MAPTRN or c_maptrn, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the ezmap man page for a description of all EZMAP error
messages and/or informational messages.
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
