.TH ARPRAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
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
Preprocessing the area map is a seven-step process to 
ensure that no edge segments cross, and to make area 
identifier information consistent in the area map. The only 
advantage of user calls to ARPRAM is to use the time-saving 
shortcuts specified by the IF1, IF2, and IF3 arguments.
.sp
Step 1: ARPRAM shortens edge segments whose 
projections on the X axis are more than twice as long 
as the average. ARPRAM does this by interpolating 
points along their lengths. This improves efficiency 
when executing other parts of the algorithm.
.sp
Step 2: ARPRAM locates all intersections of all edges 
and interpolates points along these edge segments. 
This step can take a lot of time. If you set IF1 not equal 
to 0, then pairs of edge segments are examined for 
intersections only if one of the pair has a left or a right 
area identifier that is zero or negative.
.sp
Step 3: ARPRAM removes coincident edge segments 
(edge segments with identical endpoints) from the 
area map. This is done for all edge segments in the 
area map, regardless of which group or groups 
contain the coincident edge segments.
.sp
Step 4: ARPRAM searches for and removes "dangling" 
edges (those that do not contribute to enclosing any 
area). This step is skipped if IF2 is not equal to 0.
.sp
Step 5: ARPRAM looks for holes in each edge group and 
draws connecting edge segments between the edge and 
the hole. This step is skipped if IF3 is not equal to 0.
.sp
Step 6: ARPRAM adjusts area identifier information 
in the area map. It examines all the edge segments of 
each area in each group to see what area identifier 
should be assigned to the area, then makes 
adjustments.
.sp
Step 7: Connecting lines that were inserted in step 5 
are removed from the area map.
.sp
You can put edges in an area map, preprocess it, add 
more edges, preprocess it again, and so on.
.sp
.IP "MAP(LMAP)" 12
(a workspace array, dimensioned LMAP, of type INTEGER) - 
The area-map array.
.IP "IF1" 12
(an input expression of type INTEGER) - 
If you set IF1 nonzero, ARPRAM examines a pair of edge 
segments only if one of the pair has a left or right area 
identifier that is zero or negative. This would be appropriate 
for contour lines that intersect the perimeter, but that are 
known not to intersect each other.
.IP "IF2" 12
(an input expression of type INTEGER) - 
If you set IF2 nonzero, ARPRAM does not remove 
dangling edges. This would be appropriate for contour 
lines that are known not to have any such edges. This is 
not appropriate for Ezmap boundary lines, because the 
Ezmap dataset contains some edge segments (small 
islands) that are formed by unclosed curves.
.IP "IF3" 12
(an input expression of type INTEGER) - 
If you set IF3 nonzero, 
ARPRAM speeds up the process of adjusting area 
identifiers by omitting the consideration of 
"holes" in the areas examined. This is appropriate 
for contour lines.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
eezmpa,
tareas,
tezmpa,
fsppoint.
.SH ACCESS
To use ARPRAM, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order. To use c_arpram, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, 
ncarg_gks, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the areas man page for a description of all Areas error
messages and/or informational messages.
.SH SEE ALSO
Online:
areas, areas_params, ardbpa, ardrln, aredam, argeti, argtai, 
arinam, arscam, arseti, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
