.TH MAPRS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPRS - Re-calls SET. 
.SH SYNOPSIS
CALL MAPRS 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maprs()
.SH DESCRIPTION
MAPRS has no arguments.
.SH USAGE
This routine allows you to have the SPPS routine SET re-called with the
same arguments that MAPINT would use, but executes a little faster than
MAPINT.  This is useful when you are about to call one of the
routines MAPBLA, MAPDRW, MAPFST, MAPGRD, MAPGRM, MAPIQ, MAPIQA, MAPIQM,
MAPIT, MAPITA, MAPITM, MAPLBL, MAPLOT, or MAPVEC (all of which depend on
the proper SET call's having been done), and there has been some intervening,
inappropriate, call to SET.
.SH EXAMPLES
No example is available for MAPRS.
.SH ACCESS
To use MAPRS, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_maprs, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and 
ncarg_loc, preferably in that order.
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
