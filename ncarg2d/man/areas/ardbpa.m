.\"
.\"	$Id: ardbpa.m,v 1.1 1993-03-11 16:13:27 haley Exp $
.\"
.TH ARDBPA 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ARDBPA - Produces a picture of that part of the contents of the
area map MAP which belongs to the group IGIP.  The character
string LABL will be used as a label for the picture.
.SH SYNOPSIS
CALL ARDBPA (MAP, IGIP, LABL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ardbpa (int *map, int igip, char *labl)
.SH DESCRIPTION 
.IP "MAP" 12
(Integer Array, Workspace) - 
An array containing an area map that has been
initialized by a call to ARINAM and to which edges have
been added by calls to AREDAM.
.IP "IGIP" 12
(Integer, Input) - 
The group identifier of the group which you wish to look
at.
.IP "LABL" 12
(Character String, Input) - 
The label that you would like on the plot.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH USAGE
Each edge in the plot appears in one of four different colors,
depending on whether the area identifiers to the left and right
are less than or equal to zero or greater than zero, as
follows:
.sp
.TS
tab (/);
c c c
l l l .
COLOR/LEFT AREA IDENTIFIER/RIGHT AREA IDENTIFIER
.sp
Magenta/\(<= 0/\(<=0
Yellow/\(<= 0/>0
Cyan/>0/\(<=0
White/>0/>0
.TE
.sp
In some cases you may notice gray lines in your plot.  This
means that the same edge occurs in more than one group.  In all
but one of those groups, Areas negates the group identifier for
the edge in the area map.  This allows Areas to include the
edge when it is looking at a particular group (as in ARPRAM),
but omit it when it is looking at the union of all the groups
(as in ARSCAM).
.sp
Note that you don't need to negate the group identifier.  The
negated group identifier is one of the pieces of information
that is stored with each edge in the area map.
.sp
ARDBPA should not be used for an area map that is too
complicated, as the amount of output can be pretty large.
.SH ACCESS
To use ARDBPA, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_ardbpa, load the
NCAR Graphics libraries ncargC, ncarg_gksC, @@@, ncarg, ncarg_gks, and 
ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
ardbpa, ardrln, aredam, argeti, argtai, arinam, arpram, arscam, 
arseti, areas, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation 
for Atmospheric Research
.br
All Rights Reserved
