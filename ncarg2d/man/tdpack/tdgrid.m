.TH TDGRID 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDGRID - Draw a perimeter, optionally with ticks or grid lines, around the
edges of the reference parallelogram defined by the last call to TDPARA.
This routine is normally called indirectly (by virtue of a call to TDGRDS),
but there is no reason why it should not be called directly.
.SH SYNOPSIS
CALL TDGRID (XBEG, XSTP, NOXS, YBEG, YSTP, NOYS, IGRD)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdgrid(float xbeg, float xstp, int noxs, float ybeg, float ystp,
int noys, int igrd)
.SH DESCRIPTION
The arguments of TDGRID are as follows:
.IP "XBEG, XSTP, and NOXS" 8
(input expressions of types REAL, REAL, and INTEGER, respectively) -
define where ticks or grid lines are to be drawn along the "X" axis of
the parallelogram (at XBEG, XBEG+XSTP, XBEG+2*XSTP, ... XBEG+NOXS*XSTP).
.IP "YBEG, YSTP, and NOYS" 8
(input expressions of types REAL, REAL, and INTEGER, respectively) -
define where ticks or grid lines are to be drawn along the "Y" axis of
the parallelogram (at YBEG, YBEG+YSTP, YBEG+2*YSTP, ... YBEG+NOYS*YSTP).
.IP "IGRD" 8
(an input expression of type INTEGER) -
defines what is to be drawn and has one of the values 1 (draw just a
perimeter), 2 (draw a perimeter with inward-pointing ticks), or 3
(draw a perimeter with a grid).
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDGRID or c_tdgrid, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgtrs, tdinit, tditri, tdlbla,
tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2006
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
