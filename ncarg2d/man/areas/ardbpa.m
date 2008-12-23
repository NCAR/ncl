'\" t
.TH ARDBPA 3NCARG "April 1995" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARDBPA - Produces a plot showing all of the edge segments in an
area map that have a specified group identifier IGRP; if IGRP
is less than or equal to zero, all groups are included.  Such
plots allow one to debug problems with an area map.
.SH SYNOPSIS
CALL ARDBPA (MAP,IGRP,LABEL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ardbpa (int *map, int igrp, char *label)
.SH DESCRIPTION 
.IP "MAP" 12
(an input/output array of type INTEGER) - An array containing an area map that
has at least been initialized by a call to ARINAM and to which edges will
probably have been added by calls to AREDAM.
.sp
Note: As part of initializing the area map, ARINAM stores the dimension of
MAP in MAP(1); therefore, the dimension does not have to be given as an
argument in calls to ARDBPA.)
.IP "IGRP" 12
(an input expression of type INTEGER) - 
The group identifier of the group that you want to examine.  If IGRP is less
than or equal to zero, edges from all groups will be shown.
.IP "LABEL" 12
(an input constant or variable of type CHARACTER) -
The label you want put on the plot.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
When ARDBPA is called, it draws the requested picture and then calls FRAME
to advance to a new frame.
.sp
By default, each edge segment in the plot appears in one of four different
colors, depending on whether the area identifiers to the left and 
right are less than or equal to zero or greater than zero, as follows:
.sp
.TS
tab (/);
l l l
l l l .
Color/Left area identifier/Right area identifier
-----/--------------------/---------------------
.sp
Magenta/Less than or equal to 0/Less than or equal to 0 
Yellow/Less than or equal to 0/Greater than 0
Cyan/Greater than 0/Less than or equal to 0
White/Greater than 0/Greater than 0
.TE
.sp
In some cases you may notice gray lines in your plot. This
means that the same edge occurs in more than one group. In all
but one of those groups, Areas negates the group identifier for
the edge in the area map. This allows Areas to include the
edge when it is looking at a particular group (as in ARPRAM),
but omit it when it is looking at the union of all the groups
(as in ARSCAM).
.sp
Color indices DC+1 through DC+5 are used for the required colors.  The
default value of DC is 100, so, by default, ARDBPA redefines color indices
101 through 105.  If this would result in colors that you have defined
being redefined, you should change the value of DC to something else.
.sp
Nominally, each edge segment is shown with an arrowhead, indicating the
order in which the points defining the edge segment occur in the
area map and therefore which side of the edge segment is to the left
and which side is to the right.  In regions where putting an arrowhead
on each edge segment would result in too much clutter, some of them
may be omitted.
.sp
The left and right area identifiers for each edge segment are
written in the appropriate positions relative to the edge segment.
Also, if IGRP is less than or equal to zero, the group identifier
for each edge segment is written on the segment itself.
These identifiers are intentionally written using very small
characters; the idea is that you can look at the whole plot to
get some idea of possible problem regions; when such a region
is found, you can enlarge it, using the "zoom" capability of
"idt", for a closer look; at that point, the area identifiers
become readable.
.sp
If ARDBPA is used for a complicated area map, the 
amount of output can be very large.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:
arex01,
cardb2.
.SH ACCESS
To use ARDBPA or c_ardbpa, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
areas, areas_params, ardrln, aredam, argeti, argetr, argtai, arinam,
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
