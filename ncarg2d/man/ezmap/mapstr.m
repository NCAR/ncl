.TH MAPSTR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPSTR - 
Sets the values of certain Ezmap 
parameters of type REAL. 
.sp
MPSETR is an alternate name for the routine MAPSTR.
.SH SYNOPSIS
CALL MAPSTR (PNAM,RVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapstr (char *pnam, float rval)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression, of type CHARACTER*2) specifies the name of an
internal parameter whose value is to be set. Only the first two
characters of this string are examined.
.IP RVAL 12
(an input expression of type REAL) 
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
cmpitm,
cmplab,
cmplbl,
cmplot,
cmpmsk,
cmpsat,
cmptit,
cmptra,
cmpusr,
cpex01,
cpex09,
mpex05,
mpex06,
mpex10,
mpexfi,
epltch,
tezmap,
fgkgpl,
fgkgtx,
fngngdts,
fpchiqu.
.SH ACCESS
To use MAPSTR or MPSETR, load the NCAR Graphics libraries ncarg, 
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.  To use 
c_mapstr or c_mpsetr, load the NCAR Graphics libraries ncargC, 
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
mapstc,
mapsti,
mapstl,
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
