.TH MAPSTC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPSTC - 
Sets the values of certain Ezmap 
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
Ezmap parameters.  For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpmap,
ccppole,
ccpvp,
colcon,
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
epltch,
tezmap,
tezmpa,
fgkgpl,
fgkgtx,
fngngdts,
fpchiqu.
.SH ACCESS
To use MAPSTC or MPSETC, load the NCAR Graphics libraries ncarg, 
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.  To use 
c_mapstc or c_mpsetc, load the NCAR Graphics libraries ncargC, 
ncarg_gksC, ncarg, ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the ezmap man page for a description of all Ezmap error
messages and/or informational messages.
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
Tutorial: A Step-by-Step Guide to Contouring and Mapping 
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
