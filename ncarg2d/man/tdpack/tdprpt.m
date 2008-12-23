.TH TDPRPT 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDPRPT - Given the coordinates of a point in 3-space, get the coordinates
of its projection in the projection plane.
.SH SYNOPSIS
CALL TDPRPT (UI3D, VI3D, WI3D, XI2D, YI2D)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdprpt(float ui3d, float vi3d, float wi3d, float *xi2d, float *yi2d)
.SH DESCRIPTION
.sp
The arguments of TDPRPT are as follows:
.IP "UI3D, VI3D, and WI3D" 8
(input expressions of type REAL) -
the coordinates of a point in 3-space.
.IP "XI2D and YI2D" 8
(output variables of type REAL) -
the coordinates of the projection of the point in the projection plane.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDPRPT or c_tdprpt, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdpara, tdplch, tdprpa, tdprpi, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
