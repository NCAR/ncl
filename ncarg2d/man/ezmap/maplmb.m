.TH MAPLMB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME@@@
SVG programmer nees to look at this man page.
.sp
MAPLMB - Draws a limb line.
.SH SYNOPSIS
CALL MAPLMB
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maplmb()
.SH DESCRIPTION 
In many filled or masked maps, you may need a limb line.
You can draw a limb line by calling MAPLMB.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
mpex07.
.SH ACCESS
To use MAPLMB, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_maplmb, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and 
ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
Ezmap,
Ezmap_params,
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
NCAR Graphics Contouring and Mapping Tutorial; 
"NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
