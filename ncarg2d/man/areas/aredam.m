.\"
.\"	$Id: aredam.m,v 1.1 1993-03-11 16:13:36 haley Exp $
.\"
.TH AREDAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
AREDAM - Adds edges to an area map.
.SH SYNOPSIS
CALL AREDAM (MAP, XCA, YCA, LCA, IGI, IDL, IDR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_aredam (int *map, float *xca, float *yca, int lca, int igi, int idl, int idr)
.SH DESCRIPTION 
.IP "MAP(LMAP)" 12
(Integer array, Workspace) - 
The integer area map array.
.IP "XCA(LCA)" 12
(Real array, Input) - 
The X coordinates, in the user 
coordinate system, of the points defining the edge.
.IP "YCA(LCA)" 12
(Real array, Input) - 
The Y coordinates, in the user 
coordinate system, of the points defining the edge.
.IP "LCA" 12
(Integer, Input) - 
Number of coordinates in XCA and YCA.
.IP "IGI" 12
(Integer, Input) - 
Identifier of the group to which the edge 
belongs.
.IP "IDL" 12
(Integer, Input) - 
Identifier of the area that lies to the left 
of the edge.
.IP "IDR" 12
(Integer, Input) - 
Identifier of the area that lies to the 
right of the edge.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use AREDAM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_aredam, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and 
ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ardbpa, ardrln, aredam, argeti, argtai, arinam, arpram, arscam, 
arseti, areas, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping; 
"NCAR Graphics Autograph, A Graphing Utility," Version 2.00, 
August 1987; "NCAR Graphics User's Guide," Version 2.00; and
"NCAR Graphics Guide to New Utilities," Version 3.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
