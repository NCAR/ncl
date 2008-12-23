.TH TDMTRI 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDMTRI - Add triangles defining a 3D marker to the triangles in a triangle
list.
.SH SYNOPSIS
CALL TDMTRI (IMRK, UMRK, VMRK, WMRK, SMRK, RTRI, MTRI, NTRI, IRST, UMIN,
VMIN, WMIN, UMAX, VMAX, WMAX)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdmtri(int imrk, float umrk, float vmrk, float wmrk, float smrk,
float *rtri, int mtri, int *ntri, int irst, float umin, float vmin,
float wmin, float umax, float vmax, float wmax)
.SH DESCRIPTION
The arguments of TDMTRI are as follows:
.IP "IMRK" 8
(an input expression of type INTEGER) - has an absolute value between
1 and 5, inclusive, specifying the type of marker to be generated.  If the
value of IMRK is less than zero, the triangles will not be clipped against
the sides of the data box, otherwise, they will.
.IP "UMRK, VMRK, and WMRK" 8
(input expressions of type REAL) - the 3-space coordinates of the center
point of the marker.
.IP "SMRK" 8
(an input expression of type REAL) - the radius of the marker in 3-space.
.IP "RTRI" 8
(an input array, of type REAL, dimensioned 10 x MTRI) -
a list of triangles, probably created by means of calls to TDSTRI, TDITRI,
and/or TDMTRI, and sorted, probably by means of a call to TDOTRI.
.IP "MTRI" 8
(an input expression of type INTEGER) - the second dimension of RTRI
and thus the maximum number of triangles the triangle list will hold.
.IP "NTRI" 8
(an input/output variable of type INTEGER) - keeps track of the number of
triangles currently in the list.  It is the user's responsibility to zero this
initially and its value is increased by each call to a triangle-generating
routine like TDMTRI. If NTRI becomes equal to MTRI, TDMTRI does not take an
error exit; instead, it just stops generating triangles.  Therefore, it's a
good idea, after calling TDMTRI, to check the value of NTRI against the
dimension MTRI; if they're equal, it probably means that the triangle list
filled up and that the rendered surface will be incomplete.
.IP "IRST" 8
(an input expression of type INTEGER) - specifies the index of the rendering
style to to be used for the triangles added to the triangle list by this call.
.IP "UMIN, VMIN, WMIN, UMAX, VMAX, and WMAX" 8
(input expressions of type REAL) - each of these specifies one of the
coordinate values defining the data box in 3-space.  The names of these
should make it clear what they are.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDMTRI or c_tdmtri, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
