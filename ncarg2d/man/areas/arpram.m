.\"
.\"	$Id: arpram.m,v 1.1 1993-03-11 16:13:47 haley Exp $
.\"
.TH ARPRAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ARPRAM - Preprocesses an area map that has been initialized by a
call to ARINAM and to which edges have been added by 
calls to AREDAM.
.SH SYNOPSIS
CALL ARPRAM (MAP, IF1, IF2, IF3)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_arpram (int *map, int if1, int if2, int if3)
.SH DESCRIPTION 
ARPRAM first shortens edge segments whose projections 
on the X axis are more than twice as long as the average. 
ARPRAM does this by interpolating points along their 
lengths. This leads to greater efficiency in executing 
other parts of the algorithm.
.sp 
Next, ARPRAM locates where all edges intersect each 
other and interpolates points along the edge segments 
that intersect. This can be a time-consuming operation.
.IP "MAP(LMAP)" 12
(Integer array, Workspace) - 
The area-map array.
.IP "IF1" 12
(Integer, Input) - 
If you set IF1 nonzero, ARPRAM examines 
a pair of edge segments only if one of the pair has a left 
or right area identifier that is zero or negative. This 
would be appropriate, for example, for contour lines that 
are known not to intersect each other, but that do 
intersect the perimeter. 
.sp
ARPRAM now searches for and removes "dangling" edges 
(those that do not contribute to enclosing any area). All 
such edges are automatically removed.
.IP "IF2" 12
(Integer, Input) - 
If you set IF2 nonzero, ARPRAM does not
remove dangling edges.  
Again, this would be appropriate for 
contour lines that are known not to have any such edges. 
This is not appropriate for Ezmap boundary lines. (The 
Ezmap dataset contains areas (islands for example) that 
are formed from simple unclosed curves.) 
.sp
ARPRAM now adjusts area identifier information in the 
area map. It examines all the edge segments of each area 
in each group to see what area identifier should be 
assigned to the area, and makes adjustments.
.IP "IF3" 12
(Integer, Input) - 
If you set IF3 nonzero, ARPRAM speeds up 
this process of adjusting area identifiers
by omitting the consideration 
of "holes" in the areas examined. Again, this is 
appropriate for contour lines. 
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use ARPRAM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_arpram, load the 
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
August 1987; "NCAR Graphics User's Guide," Version 2.00, and
"NCAR Graphics Guide to New Utilities," Version 3.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
