.TH MAPINT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPINT - 
Initialization.
.SH SYNOPSIS
CALL MAPINT 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapint()
.SH DESCRIPTION
MAPINT has no arguments.
.SH USAGE 
MAPINT is required initially and again after a call to any of
the routines MAPPOS, MAPROJ, or MAPSET. The flag 'IN', which
may be retrieved by a call to MAPGTI or MAPGTL, indicates
whether or not initialization is required at a given time.  No
parameter change by means of a call to MAPSTx forces
reinitialization; reinitialization is forced only by 
calls to MAPPOS, MAPROJ, and MAPSET. ("MAPSTx" refers to any
routine that starts with the characters "MAPST".)
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpcica,
ccpmovi,
ccppole,
colcon,
cmpclr,
cmpdd,
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
cmptit,
cpex08,
mpex09,
mpex10,
eezmpa,
epltch,
tezmpa,
fcover,
ffex00,
ffex02,
ffex03,
ffex05,
fpchiqu.
.SH ACCESS
To use MAPINT, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_mapint, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
ncarg_c, and ncarg_loc, preferably in that order.
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
