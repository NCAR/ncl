.TH TDLNPA 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDLNPA - Given the coordinates of two points in the reference parallelogram,
draw the projection of the line joining them.
.SH SYNOPSIS
CALL TDLNPA (XCP1, YCP1, XCP2, YCP2)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdlnpa(float xcp1, float ycp1, float xcp2, float ycp2)
.SH DESCRIPTION
.sp
The arguments of TDLNPA are as follows:
.IP "XCP1 and YCP1" 8
(input expressions of type REAL) -
the parallelogram coordinates of a point in the reference parallelogram (as
defined by the last call to TDPARA).
.IP "XCP2 and YCP2" 8
(input expressions of type REAL) -
the parallelogram coordinates of another point in the reference parallelogram.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDLNPA or c_tdlnpa, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
