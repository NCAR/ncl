.TH TDPRPA 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDPRPA - Given the parallelogram coordinates of a point, get the coordinates
of its projection in the projection plane.  This routine is essentially the
inverse of the routine TDPRPI.
.SH SYNOPSIS
CALL TDPRPA (XIPA, YIPA, XI2D, YI2D)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdprpa(float xipa, float yipa, float *xi2d, float *yi2d)
.SH DESCRIPTION
.sp
The arguments of TDPRPA are as follows:
.IP "XIPA and YIPA" 8
(input expressions of type REAL) -
the parallelogram coordinates of a point.  (The parallelogram is as defined
by a prior call to TDPARA.)
.IP "XI2D and YI2D" 8
(output variables of type REAL) -
the coordinates of the projection of the point in the projection plane.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDPRPA or c_tdprpa, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdpara, tdplch, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
