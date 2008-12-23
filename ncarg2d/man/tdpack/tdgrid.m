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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
