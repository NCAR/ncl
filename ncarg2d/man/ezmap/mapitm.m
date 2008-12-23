.TH MAPITM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPITM - Draws lines on a map, masked by a specified area map.
.SH SYNOPSIS
 CALL MAPITM (RLAT,RLON,IFST,IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,
+ NOGI,ULPR)
.sp
CALL MAPIQM (IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,NOGI,ULPR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapitm (float rlat, float rlon, int ifst, 
.br
int *iama, float *xcra, float *ycra, int mcra, 
.br
int *iaai, int *iagi, int nogi, 
.br
int (*ulpr)(float *xcra, 
float *ycra, int *mcra, 
.br
int *iaai, int *iagi, int *nogi))
.sp
void c_mapiqm (int *iama, float *xcra, float *ycra, 
.br
int mcra, int *iaai, int *iagi, int nogi, 
.br
int ulpr(float *xcra, 
float *ycra, int *mcra, 
.br
int *iaai, int *iagi
, int *nogi))
.IP "RLAT and RLON" 12 
(input expressions, of type REAL) specify the latitude and
longitude of a point to which the "pen" is to be moved. Both are given in
degrees. RLAT must be between -90. and +90., inclusive; RLON must be
between -540. and +540., inclusive.
.IP "IFST" 12 
(an input expression, of type INTEGER) is 0 to do a "pen-up" move, 1
to do a "pen-down" move only if the distance from the last point to the
new point is greater than 'MV' plotter units, and 2 or greater to do a
"pen-down" move regardless of the distance from the last point to the new
one.
.IP "IAMA" 12
(an input/output array of type INTEGER, dimensioned as specified in
a call to the AREAS initialization routine ARINAM) is the array
containing the area map against which lines drawn by MAPIQM will be
masked.
.IP "XCRA and YCRA" 12 
(scratch arrays of type REAL, each dimensioned MCRA) are to
be passed by MAPIQM to the AREAS routine ARDRLN, which uses them in calls
to the user line-processing routine ULPR. They will hold the X and Y
coordinates of points in the fractional coordinate system defining some
portion of the projection of a user-defined polyline on the globe.
.IP MCRA 12
(an input expression of type INTEGER) is the size of each of the
arrays XCRA and YCRA. The value of MCRA must be at least two. For most
applications, the value 100 works nicely.
.IP "IAAI and IAGI" 12
(scratch arrays of type INTEGER, each dimensioned NOGI) are
to be passed by MAPIQM to the AREAS routine ARDRLN, which uses them in
calls to the user line-processing routine ULPR. They will hold area
identifier/group identifier pairs for the area containing the polyline
fragment defined by XCRA and YCRA.
.IP NOGI 12
(an input expression of type INTEGER) is the size of each of the
arrays IAAI and IAGI. The value of NOGI must be greater than or equal to
the number of groups of edges placed in the area map in IAMA.
.IP ULPR 12
is the name of a user-provided line-processing routine. This name
must appear in an EXTERNAL statement in the routine that calls MAPITM, so
that the compiler and loader will know that it is the name of a routine
to be called, rather than the name of a variable.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
The routines MAPITM and MAPIQM may be used to draw lines defined by a series
of user-specified latitudes and longitudes on a map, masked by the areas
defined by an area map (perhaps one created by a call to MAPBLA or
perhaps one created by a set of calls to routines in the package AREAS);
like MAPIT and MAPIQ, they attempt to omit non-visible portions of lines
and to handle "cross-over".
.sp
MAPITM is called like the EZMAP routine MAPIT (which see, above) but has
some additional arguments:
.sp
.RS 4
 CALL MAPITM (RLAT,RLON,IFST,IAMA,XCRA,YCRA,MCRA,IAAI, 
.br
+ IAGI,NOGI,ULPR)
.RE
.sp
Additional arguments are the area-map array IAMA, coordinate arrays XCRA
and YCRA, dimensioned MCRA, integer arrays IAAI and IAGI, dimensioned
NOGI, and a user-provided line-processing routine named ULPR. MAPIQM is
called like the EZMAP routine MAPIQ, to terminate a series of calls to
MAPITM and to flush the buffers; it has the same additional arguments:
.sp
.RS 4
CALL MAPIQM (IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,NOGI,ULPR)
.RE
.sp
The additional arguments are passed by MAPITM and MAPIQM to the routine
ARDRLN, in the package named AREAS. 
.sp
For more information, see the man page for the routine MAPIT.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
cmpitm. 
.SH ACCESS
To use MAPITM or c_mapitm, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
