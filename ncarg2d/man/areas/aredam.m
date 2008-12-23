.TH AREDAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
AREDAM - Adds edge segments to an area map.
.SH SYNOPSIS
CALL AREDAM (MAP,XCA,YCA,LCA,IGI,IDL,IDR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_aredam (int *map, float *xca, float *yca, int lca, int igi, 
int idl, int idr)
.SH DESCRIPTION 
.IP "MAP" 12
(an input/output array of type INTEGER) - An array containing an area map that
has been initialized by a call to ARINAM.
.sp
Note: As part of initializing the area map, ARINAM stores the dimension of
MAP in MAP(1); therefore, the dimension does not have to be given as an
argument in calls to AREDAM.)
.IP "XCA" 12
(an input array, dimensioned LCA, of type REAL) - 
The X coordinates, in the user 
coordinate system, of the points defining an edge.
.IP "YCA" 12
(an input array, dimensioned LCA, of type REAL) - 
The Y coordinates, in the user 
coordinate system, of the points defining an edge.
.IP "LCA" 12
(an input expression of type INTEGER) - 
The number of coordinates in XCA and YCA.
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
.SH USAGE
The area-map array MAP must have been initialized by a call to ARINAM.
Each subsequent call to AREDAM adds to the area map the LCA-1 edge segments
defined by the points whose user coordinates are in the arrays XCA and YCA.
(Points 1 and 2 define the first edge segment, points 2 and 3 define the
next edge segment, points 3 and 4 the next one, and so on.)
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
To use AREDAM or c_aredam, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, argeti, argetr, argtai, arinam,
armvam, arpram, arscam, arseti, arsetr, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
