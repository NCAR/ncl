.TH TDLINE 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDLINE - Given the coordinates of two points in 3-space, draw the projection
of the line joining them.
.SH SYNOPSIS
CALL TDLINE (UCP1, VCP1, WCP1, UCP2, VCP2, WCP2)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdline(float ucp1, float vcp1, float wcp1, float ucp2, float vcp2,
float wcp2)
.SH DESCRIPTION
.sp
The arguments of TDLINE are as follows:
.IP "UCP1, VCP1, and WCP1" 8
(input expressions of type REAL) -
the coordinates of a point in 3-space.
.IP "UCP2, VCP2, and WCP2" 8
(input expressions of type REAL) -
the coordinates of another point in 3-space.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDLINE or c_tdline, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
