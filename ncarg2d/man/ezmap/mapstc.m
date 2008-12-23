.TH MAPSTC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPSTC - 
Sets the values of certain EZMAP
parameters of type CHARACTER. 
.sp
MPSETC is an alternate name for the routine MAPSTC.
.SH SYNOPSIS
CALL MAPSTC (PNAM,CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapstc(char *pnam, char *cval)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression, of type CHARACTER*2) specifies the name of an
internal parameter whose value is to be set. Only the first two
characters of this string are examined.
.IP CVAL 12
(an input expression of type CHARACTER) 
contains the value to be given to
the parameter specified by PNAM.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current value of
EZMAP parameters.  For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cezmap1,
cezmap2,
cezmap3,
cmpclr,
cmpdd,
cmpdrw,
cmpel,
cmpfil,
cmpgrd,
cmpgrp,
cmpita,
cmpitm,
cmplab,
cmplbl,
cmplot,
cmpmsk,
cmpou,
cmpsat,
cmptit,
cmptra,
cmpusr,
cpex01,
cpex03,
cpex08,
cpex09,
mpex01,
mpex02,
mpex04,
mpex05,
mpex06,
mpex07,
mpex09,
eezmpa,
tezmap,
tezmpa.
.SH ACCESS
To use MAPSTC, MPSETC, or c_mapstc, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.
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
