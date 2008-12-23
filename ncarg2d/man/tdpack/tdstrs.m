.TH TDSTRS 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDSTRS - Set the values defining a selected rendering style, which affects the
appearance of triangles drawn by a call to the routine TDDTRI.
.SH SYNOPSIS
CALL TDSTRS (IRST, IFC1, IFC2, IFC3, IFC4, ILC1, ILC2, ILTD, USTP, VSTP, WSTP)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdstrs(int irst, int ifc1, int ifc2, int ifc3, int ifc4, int ilc1,
int ilc2, int iltd, float ustp, float vstp, float wstp)
.SH DESCRIPTION
The arguments of TDSTRS are as follows:
.IP "IRST" 8
(an input expression of type INTEGER) - the index of the particular rendering
style which is to be retrieved.
.IP "IFC1 and IFC2" 8
(output variables of type INTEGER) -
color indices specifying a range of colors to be used
for the "bottom" side of a surface (where function values are less than
the value on the surface). If IFC1 is negative, filling of triangles seen
from the "bottom" is turned off. If IFC1 is zero or greater, but IFC2 is
less than or equal to it, the color with index IFC1 is used. If IFC1 is zero
or greater and IFC2 is greater than IFC1, then a range of color indices
is specified; colors near the beginning of that range are used for
triangles that are nearly perpendicular to the line of sight, while colors
near the end of that range are used for triangles more nearly parallel
to the line of sight. (Normally, one should make triangles
perpendicular to the line of sight lighter than those parallel to the line
of sight.)
.IP "IFC3 and IFC4" 8
(output variables of type INTEGER) -
color indices specifying a range of colors to be used
for the "top" side of a surface (where function values are greater than
the value on the surface). If IFC3 is negative, filling of triangles seen
from the "top" is turned off. If IFC3 is zero or greater, but IFC4 is less
than or equal to it, the color with index IFC3 is used. If IFC3 is zero or
greater and IFC4 is greater than IFC3, then a range of color indices is
specified; colors near the beginning of that range are used for triangles
that are nearly perpendicular to the line of sight, while colors near the
end of that range are used for triangles more nearly parallel to the line
of sight. (Normally, one should make triangles perpendicular to the
line of sight lighter than those parallel to the line of sight.)
.IP "ILC1" 8
(an output variable of type INTEGER) -
the color index specifying a color to be used for lines drawn on
the "bottom" side of a surface.  If ILC1 is negative, the drawing of these
lines is turned off.
.IP "ILC2" 8
(an output variable of type INTEGER) -
the color index specifying a color to be used for lines drawn on
the "top" side of a surface.  If ILC2 is negative, the drawing of these lines
is turned off.
.IP "ILTD" 8
(an output variable of type INTEGER) -
a flag, which, if set non-zero, turns on the drawing of the
edges of the individual triangles into which surfaces have been
decomposed.
.IP "USTP, VSTP, and WSTP" 8
(output variables of type REAL) - the distances between slices in the U, V,
and W directions, respectively. If a given value is zero, the associated
slice lines are not drawn.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDSTRS or c_tdstrs, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdpara, tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
