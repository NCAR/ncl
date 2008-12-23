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
void c_mappos (float xlow, float xrow, float ybow, 
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
EZMAP parameters 'XL', 'XR', 'YB', and 'YT'.
For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
caredg,
ccpcica,
ccpmovi,
ccppole,
ccpvp,
cmppos,
cmptit,
cpex03,
cpex08,
mpex04,
mpex05,
mpex06,
mpex07,
epltch,
fgkgpl,
fpchiqu.
.SH ACCESS
To use MAPPOS or c_mappos, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
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
