.TH MAPDRW 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPDRW - 
Draws the complete map described by the current values of the internal
parameters of Ezmap.
.SH SYNOPSIS
CALL MAPDRW 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapdrw() 
.SH DESCRIPTION
MAPDRW does not use any arguments. 
.SH USAGE@@@
MAPDRW calls MAPINT (if the value of the internal parameter 'IN' indicates
that initialization is required), MAPGRD, MAPLBL, and MAPLOT,
in that order. The user may wish to call these routines
directly.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpmap,
ccpvp,
cezmap1,
cezmap2,
cezmap3,
cmpdrw,
cmppos,
cmpsat,
cmptra,
cmpusr,
cpex01,
cpex03,
cpex09,
mpex01,
mpex02,
mpex04,
mpex05,
mpex06,
mpex07,
mpex10,
tezmap,
fgkgpl,
fgkgtx,
fngngdts.
.SH ACCESS
To use MAPDRW, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_mapdrw, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ezmap, 
ezmap_params, 
mapaci,
mapbla,
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
