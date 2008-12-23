.TH TDCTRI 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDCTRI - Cut triangles defined by a triangle list.
.SH SYNOPSIS
CALL TDCTRI (RTRI, MTRI, NTRI, IAXS, RCUT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdctri(float *rtri, int mtri, int *ntri, int iaxs, float rcut)
.SH DESCRIPTION
This routine, given a list of NTRI triangles in the array RTRI, an axis
identifier IAXS (1 => U axis, 2 => V axis, 3 => W axis), and a value RCUT
along that axis, cuts all the triangles that intersect the plane perpendicular
to the identified axis at the specified value into two pieces, one on either
side of the plane.  This is useful when one is rendering the triangles in
colors that depend on their position in U, V, or W.  (See the example "tdex04".)
.sp
The arguments of TDCTRI are as follows:
.IP "RTRI" 8
(an input/output array, of type REAL, dimensioned 10 x MTRI) -
a list of triangles, probably created by means of calls to TDSTRI, TDITRI,
and/or TDMTRI.  The number of triangles in the list may increase as a result
of calling TDCTRI.
.IP "MTRI" 8
(an input expression of type INTEGER) - the second dimension of RTRI
and thus the maximum number of triangles the triangle list will hold.
.IP "NTRI" 8
(an input/output variable of type INTEGER) - specifies the number of triangles
currently in the list.  It is the user's responsibility to zero this
initially; its value is increased by each call to a triangle-generating
routine like TDSTRI or TDITRI and may be increased by a call to TDCTRI.
.IP "IAXS" 8
(an input variable of type INTEGER) - specifies the axis to which the cut
plane is perpendicular (1 => U axis, 2 => V axis, 3 => W axis).
.IP "RCUT" 8
(an input variable of type REAL) - specifies that value of U, V, or W at
which the cut plane intersects the axis.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDCTRI or c_tdctri, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
