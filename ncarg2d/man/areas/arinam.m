.\"
.\"	$Id: arinam.m,v 1.1 1993-03-11 16:13:44 haley Exp $
.\"
.TH ARINAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ARINAM - Initializes area map.
.SH SYNOPSIS
CALL ARINAM (MAP, LMAP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_arinam (int *map, int lmap)
.SH DESCRIPTION
.IP "MAP(LMAP)"
(Integer array, Workspace) - 
An integer array in which an 
area map is to be constructed. Each point on your map 
requires ten words in the array MAP, and additional 
points will be added to these. For small area maps, MAP 
can be dimensioned at 5,000; for area maps with many 
areas (such as global maps), MAP should be at least 
50,000.
.IP "LMAP"
(Integer, Input) - 
Length of the MAP array. 
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use ARINAM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_arinam, load the 
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
August 1987"; "NCAR Graphics User's Guide," Version 2.00, and
"NCAR Graphics Guide to New Utilities," Version 3.00 
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
