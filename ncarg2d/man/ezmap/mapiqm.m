.TH MAPIQM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPIQM - Terminates a string of calls to the routine MAPITM.
.SH SYNOPSIS
CALL MAPIQM (IAMA,XCRA,YCRA,MCRA,IAAI,IAGI,NOGI,ULPR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapiqm (int *iama, float *xcra, float *ycra, 
.br
int mcra, int *iaai, int *iagi, int nogi, 
.br
int (*ulpr)(float *xcra, float *ycra, int *mcra, 
.br
int *iaai, int *iagi, int *nogi))
.SH DESCRIPTION 
.IP IAMA 12 
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
You must call MAPITM once for each point along the line.  After
the last call to MAPITM for a given line, you must call MAPIQM
to signal the end of the line.
.sp
For more information, see the man pages for the routines MAPIT and MAPITM.
SH EXAMPLES
Use the ncargex command to see the following relevant
example:
cmpitm,
.SH ACCESS
To use MAPIQM or c_mapiqm, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
