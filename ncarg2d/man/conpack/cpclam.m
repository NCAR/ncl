.TH CPCLAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPCLAM - Adds contour lines to an area map. This is part of the
process of drawing a solid-fill contour plot.
.SH SYNOPSIS
CALL CPCLAM (ZDAT, RWRK, IWRK, IAMA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpclam (float *zdat, float *rwrk, int *iwrk, \\
.br
int *iama)
.SH DESCRIPTION 
The first three arguments are arrays used in the last call 
to CPRECT, CPSPS1, or CPSPS2, the contents of which must 
not have been changed since that call.
.IP ZDAT 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input) is the data array.
.IP RWRK 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input/output) is the real 
workspace array.
.IP IWRK 12
(INTEGER array, dimensioned as specified in the last 
call to CPRECT, CPSPS1, or CPSPS2, input/output) is the 
integer workspace array.
.IP IAMA 12
(INTEGER array, dimensioned as specified in a call to 
ARINAM, in the package Areas) is the array containing the 
area map to which contour lines are to be added.
.SH USAGE@@@
The routine CPCLAM, which adds contour lines generated from the
data in the array ZDAT to the area map in the array IAMA, may
be called at any time after the initialization call to CPRECT,
CPSPS1, or CPSPS2. The area map must previously have been
initialized by a call to the routine ARINAM in the utility
Areas.
.sp
The contour lines added to the area map are as specified by the
first 'NCL' elements of the parameter arrays 'CLV', 'AIA', and
\&'AIB'. If 'NCL' is zero, CPPKCL is called to generate these
values; if 'NCL' is still zero after the call to CPPKCL, a
fatal error results.
.sp
If, for a given value of I between 1 and 'NCL', inclusive, the
Ith element of either 'AIA' or 'AIB' is non-zero, then the
contour lines specified by the Ith element of 'CLV' are added
to the area map, with the Ith element of 'AIA' as the area
identifier for the area "above" each line (where field values
are greater than they are along the line) and with the Ith
element of 'AIB' as the area identifier for the area "below"
each line (where field values are less than they are along the
line). 
<<< confusing, very long sentence <<<
If the parameter 'T2D' has a non-zero value, the contour
lines are smoothed, using cubic splines under tension.
.sp
Four other types of lines are added to the area map by CPCLAM:
(1) the edge of the current viewport and possibly a set of
vertical lines within the viewport, (2) the edge of the grid,
(3) the edges of special-value areas, if any, and (4) the edges
of out-of-range areas, if any. The area identifier for the
outside of the viewport is always -1. You can use elements of
the parameter array 'AIA' for 'PAI' = -1, -2, and -3 to specify
the area identifiers to be used for the outside of the grid,
the inside of a special-value area, and the inside of an
out-of-range area, respectively; the default values of all
three are -1's. Area identifiers for all other sides of these
edges are determined from the area-identifier information given
for the contour levels.
.sp
Lines are added to the area map in the following order:
.IP \(bu 3
the edge of the viewport and the vertical lines within it,
.IP \(bu 3
the edges of the out-of-range areas, if any,
.IP \(bu 3
the edge of the grid,
.IP \(bu 3
the edges of the special-value areas, if any, and,
.IP \(bu 3
the contour lines, in order of increasing contour level.
.PP
The edge of the viewport may actually be added to the area map
twice:
.IP \(bu 3
as part of the edge group with group identifier 'GIC', and
.IP \(bu 3
as part of the edge group with group identifier 'GIS'.
.PP
The object of number 1 above is to prevent problems that arise
when mapping is turned on and the mapping function has a line
of discontinuity (for example, when using Ezmap with a
cylindrical equidistant projection). The object of number 2
above is to break up the areas represented by the area map into
smaller pieces. Whether this is done or not is under your
control, by means of the internal parameters 'NVS' and 'GIS'.
For more information, see the descriptions of those internal
parameters in the conpack_params.
.sp
If, during the last call to CPRECT, CPSPS1 or CPSPS2, the data
being contoured were found to be essentially constant, then no
contour lines are added to the area map; the other lines are
added, however.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cpex01,
cpex02,
cpex03,
cpex04,
cpex05,
cpex07,
cpex08,
tconpa.
.SH ACCESS
To use CPCLAM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_cpclam, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online: 
conpack, 
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cppkcl, cppklb, cprect, cprset, cpscae, cpsetc, cpseti,
cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

