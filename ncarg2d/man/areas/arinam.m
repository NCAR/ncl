.TH ARINAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARINAM - Initializes an area map.
.SH SYNOPSIS
CALL ARINAM (MAP,LMAP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_arinam (int *map, int lmap)
.SH DESCRIPTION
.IP "MAP" 12
(output array, of type INTEGER, dimensioned at least LMAP) - 
An integer array in which 
an area map is to be constructed. Each vertex for 
an edge segment in your area map requires ten 
words in the array MAP. Remember that the total 
number of vertices includes those added at each
intersection of edges, and those added when long 
edge segments are broken into shorter edge 
segments.
.IP "LMAP" 12
(an input expression of type INTEGER) - 
Length of the MAP array.  As part of the initialization process, the
value of LMAP is stored in MAP(1); that way, it need not be given as
an argument in calls to other routines that use the area map.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cardb1,
cardb2,
carfill,
carline,
carmap,
ccpcldm,
ccpfil,
ccplbam,
ccpllb,
ccpllc,
ccplll,
ccpllo,
ccpllp,
ccpllt,
ccpllw,
ccppc,
ccppc1,
ccppc2,
ccppc3,
ccppc4,
ccprc,
ccpscam,
ccpvs,
colcon,
cmpfil,
cmpgrp,
cmpita,
cmpitm,
cmplab,
cmpmsk,
cmptit,
cpex01,
cpex02,
cpex03,
cpex04,
cpex05,
cpex06,
cpex07,
cpex08,
eezmpa,
arex01,
cbex01,
vvex01,
tareas,
tconpa,
tezmpa,
fcover,
ffex00,
ffex03,
ffex05,
fcirc,
fsfsgfa,
fsppoint.
.SH ACCESS
To use ARINAM or c_arinam, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, aredam, argeti, argetr, argtai,
armvam, arpram, arscam, arseti, arsetr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2003
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
