.TH MAPPOS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPPOS - Positions the map on the plotter frame.
.SH SYNOPSIS
CALL MAPPOS (XLOW,XROW,YBOW,YTOW)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mappos (float xlow, float xrow, float ybow, \\
.br
float ytow)
.SH DESCRIPTION 
.IP XLOW 12 
(an input expression, of type REAL) specifies the position of the
left edge of the window. The default is .05.
.IP XROW 12
(an input expression, of type REAL) specifies the position of the
right edge of the window. The default is .95.
.IP YBOW 12
(an input expression, of type REAL) specifies the position of the
bottom edge of the window. The default is .05.
.IP YTOW 12
(an input expression, of type REAL) specifies the position of the
top edge of the window. The default is .95.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current values of the four
Ezmap parameters 'XL', 'XR', 'YB', and 'YT'.
For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cmppos,
cmptit,
cpex03,
cpex08,
mpex04,
mpex05,
mpex06,
mpex07,
.SH ACCESS
To use MAPPOS, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_mappos, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.
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
