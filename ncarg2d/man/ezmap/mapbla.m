.TH MAPBLA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPBLA - 
Adds to the area map in the array IAMA the set of boundary lines, of
projected geographical entities, determined by the current state of the
internal parameters of Ezmap.
.SH SYNOPSIS
CALL MAPBLA (IAMA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapbla (int *iama)
.SH DESCRIPTION 
.IP IAMA 12
(an input/output array of type INTEGER) is the area map array to
which boundary lines are to be added.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
One or two groups of boundary lines are added to the area map
by a call to MAPBLA. The first group has the group identifier
\&'G1', which has a default value of 1. The group 'G1' consists of a
perimeter (either rectangular or elliptical, depending on the
value of the Ezmap parameter 'EL') and the set of projected
boundary lines implied by your selection of an Ezmap dataset
(some combination of continental, U.S. state, and international
political outlines). 
For certain projections a limb line may also be
included. (The limb is the boundary between a projectable
region and an unprojectable one.)
.sp
If the Ezmap parameter 'VS' has a value greater than zero, a
second group, with the group identifier 'G2', is added to the area
map. (The default value of 'VS' is 1; the default value of 'G2'
is 2.) The purpose of the group 'G2' is to split up areas and
thus reduce the number of points required to define a typical
area. This may be necessary because some hardware devices fail if
the number of points defining an area is too large. The group
\&'G2' consists of a copy of the perimeter and the limb line (if
any), plus a set of vertical lines that split the area inside
the perimeter into 'VS' vertical strips.
.sp
The perimeter and the limb in the groups 'G1' and 'G2' have the
following left and right area identifiers:
.RS 8
.IP 0 4 
Identifies the area inside the perimeter or limb.
.IP -1 4 
Identifies the area outside the perimeter or limb.
.RE
.sp
The vertical lines in the group 'G2' have left and right area
identifiers of 0.
.sp
To set the values of 'G1', 'G2', and 'VS', call the Ezmap
routine MAPSTI. To get the current values of 'G1', 'G2', and
\&'VS', call the Ezmap routine MAPGTI. See the man pages for 
MAPSTI and MAPGTI.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccppole,
colcon,
cmpfil,
cmpgrp,
cmpita,
cmpitm,
cmplab,
cmpmsk,
cmptit,
cpex08,
eezmpa,
tezmpa,
fcover,
ffex00.
.SH ACCESS
To use MAPBLA, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_mapbla, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ezmap, 
ezmap_params, 
mapaci,
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
NCAR Graphics Contouring and Mapping Tutorial 
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
