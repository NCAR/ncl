.TH TDPRPI 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDPRPI - Given a point in the projection plane, retrieve the parallelogram
coordinates of that point in the reference parallelogram (as defined by
the last call to TDPARA) that projects into it.  This routine is essentially
the inverse of the routine TDPRPA.
.SH SYNOPSIS
CALL TDPRPI (XI2D, YI2D, XIPA, YIPA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdprpi(float xi2d, float yi2d, float *xipa, float *yipa)
.SH DESCRIPTION
.sp
The arguments of TDPRPI are as follows:
.IP "XI2D and YI2D" 8
(input expressions of type REAL) -
the coordinates of a point in the projection plane.
.IP "XIPA and YIPA" 8
(output variables of type REAL) -
the parallelogram coordinates of that point in the reference parallelogram
that projects into (XI2D,YI2D).  (The parallelogram is as defined by a prior
call to TDPARA.)
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDPRPI or c_tdprpi, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdpara, tdplch, tdprpa, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2003
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
