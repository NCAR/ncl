.TH AREDAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AREDAM - Adds edges to an area map.
.SH SYNOPSIS
CALL AREDAM (MAP, XCA, YCA, LCA, IGI, IDL, IDR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_aredam (int *map, float *xca, float *yca, int lca, int igi, 
int idl, int idr)
.SH DESCRIPTION 
.IP "MAP(LMAP)" 12
(a workspace array, dimensioned LMAP, of type INTEGER) - 
The integer area map array to which edge segments are to be added.
.IP "XCA(LCA)" 12
(an input array, dimensioned LCA, of type REAL) - 
The X coordinates, in the user 
coordinate system, of the points defining the edge.
.IP "YCA(LCA)" 12
(an input array, dimensioned LCA, of type REAL) - 
The Y coordinates, in the user 
coordinate system, of the points defining the edge.
.IP "LCA" 12
(an input expression of type INTEGER) - 
Number of coordinates in XCA and YCA.
.IP "IGI" 12
(an input expression of type INTEGER) - 
Identifier of the group to which the edge 
belongs.
.IP "IDL" 12
(an input expression of type INTEGER) - 
Identifier of the area that lies to the left 
of the edge.
.IP "IDR" 12
(an input expression of type INTEGER) - 
Identifier of the area that lies to the 
right of the edge.
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
arex01,
cbex01,
tareas,
fsppoint.
.SH ACCESS
To use AREDAM, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order. To use c_aredam, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, 
ncarg_gks, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, argeti, argtai, arinam, 
arpram, arscam, arseti, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
