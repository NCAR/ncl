'\" t
.TH ARDBPA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
ARDBPA - Produces a plot showing all the edges in an 
area map with a specified group identifier. In complex
area maps, this allows you to debug a problem, group
by group.
.SH SYNOPSIS
CALL ARDBPA (MAP, IGRP, LABEL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ardbpa (int *map, int igrp, char *label)
.SH DESCRIPTION 
.IP "MAP(LMAP)" 12
(a workspace array, dimensioned LMAP, of type INTEGER) - 
An array containing an area map that has been
initialized by a call to ARINAM and to which edges have
been added by calls to AREDAM.
.IP "IGRP" 12
(an input expression of type INTEGER) - 
The group identifier of the group that you want to examine.
.IP "LABEL" 12
(an input expression of type CHARACTER) - 
The label you want on the plot.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
By default, each edge in the plot appears in one of four different 
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
You can adjust these colors with the DC parameter.
.sp
In some cases you may notice gray lines in your plot. This
means that the same edge occurs in more than one group. In all
but one of those groups, Areas negates the group identifier for
the edge in the area map. This allows Areas to include the
edge when it is looking at a particular group (as in ARPRAM),
but omit it when it is looking at the union of all the groups
(as in ARSCAM).
.sp
Note that you don't need to negate the group identifier. The
negated group identifier is one of the pieces of information
that is stored with each edge in the area map.
.sp
If ARDBPA is used for a complicated area map, the 
amount of output can be very large.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
cardb2.
.SH ACCESS
To use ARDBPA, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order. To use c_ardbpa, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, and 
ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
areas, areas_params, ardrln, aredam, argeti, argtai, arinam, 
arpram, arscam, arseti, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
