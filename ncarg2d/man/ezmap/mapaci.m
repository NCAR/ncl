.TH MAPACI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPACI - 
Normally used to pick colors for the areas created
by the boundary lines added to an area map by a call to MAPBLA.
.SH SYNOPSIS
ICIR = MAPACI(IAID)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_mapaci (int iaid)
.SH DESCRIPTION 
.IP IAID 12 
(an input expression of type INTEGER) is an area identifier for one
of the areas defined by the set of boundary lines added to an area map by
a call to MAPBLA.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE@@@
ICIR = MAPACI (IAID)
.sp
Gives ICI a value between 1 and 7, inclusive, that may be used to select
a color index for the area whose area identifier is IAID.  It is guaranteed
that MAPACI will not return the same value for adjacent areas; this fact
can be used to ensure that adjacent areas will have different colors.
.sp
Note that it is the responsibility of the user to define the colors
associated with color indices
used; this is done by calling the GKS routine GSCR.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: eezmpa.
.SH ACCESS
To use MAPACI, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_mapaci, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ezmap, 
ezmap_params, 
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
