.TH MAPIQ 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPIQ - Terminates a string of calls to MAPIT.
.SH SYNOPSIS
CALL MAPIQ 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapiq()
.SH DESCRIPTION
MAPIQ has no arguments.
.SH USAGE 
MAPIQ flushes MAPIT's buffers.  It is particularly important
that this be done before a STOP or a CALL FRAME, and before
changing the color index, dash pattern, etc.
.sp
For more information,
see the man page for MAPIT.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cmpgci,
cmptra,
mpexfi.
.SH ACCESS
To use MAPIQ or c_mapiq, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
.sp
Hardcopy: 
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
