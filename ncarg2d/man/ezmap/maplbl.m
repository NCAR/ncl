.TH MAPLBL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPLBL - Labels the map.
.SH SYNOPSIS
CALL MAPLBL 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maplbl()
.SH DESCRIPTION
MAPLBL has no arguments.
.SH USAGE 
If the parameter 'LA' is set appropriately, MAPLBL labels the
international date line (ID), the equator (EQ), the Greenwich
Meridian (GM), and the poles (NP and SP), and if the parameter
\&'PE' is set appropriately, it draws the perimeter of the map.
If EZMAP needs initialization or if there is an uncleared NCAR Graphics
error, MAPLBL does nothing.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cmpclr,
cmpel,
cmpfil,
cmpgrp,
cmpita,
cmpitm,
cmplab,
cmplbl,
cmplot,
cmpmsk,
cmpou,
cmptit,
eezmpa,
tezmpa,
ffex00.
.SH ACCESS
To use MAPLBL or c_maplbl, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
