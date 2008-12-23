.TH TDGRDS 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDGRDS - Draw perimeters, optionally with ticks or grid lines, on the six
sides of a box. This routine calls TDPARA and will therefore change the
definition of the reference parallelogram.
.SH SYNOPSIS
CALL TDGRDS (UMIN, VMIN, WMIN, UMAX, VMAX, WMAX, USTP, VSTP, WSTP, IGRT, IHID)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdgrds(float umin, float vmin, float wmin, float umax, float vmax,
float wmax, float ustp, float vstp, float wstp, int igrt, int ihid)
.SH DESCRIPTION
The arguments of TDGRDS are as follows:
.IP "UMIN, VMIN, WMIN, UMAX, VMAX, WMAX" 8
(input expressions of type REAL) - coordinate values defining the box in
3-space.  The names of these should make it clear what they are.
.IP "USTP, VSTP, and WSTP" 8
(input expressions of type REAL) - specify step sizes between ticks or grid
lines in the U direction, the V direction, and the W direction, respectively.
If one of these values is less than or equal to zero, the ticks or grid lines
in the associated direction are omitted.
.IP "IGRT" 8
(an input expression of type INTEGER) - of the form 10*IGRN+IGRF, where
IGRN is a value specifying what to draw on the near sides of the box
and IGRF is a value specifying what to draw on the far sides of the box,
where "near" and "far" are as defined by the current line of sight.  Each
of IGRN and IGRF can have one of the values 0 (draw nothing), 1
(draw just a perimeter), 2 (draw a perimeter with inward-pointing
ticks), or 3 (draw a perimeter with a grid). For example, to draw grids
on the far side of the box and just perimeters on the near sides of the
box, use IGRT = 13.
.IP "IHID" 8
(an input expression of type INTEGER) - set to 0 to draw only those sides
of the box that cannot be hidden by something inside the box or to 1 to
draw only those sides of the box that can be hidden by something inside
the box.  Standard operating procedure is to call TDGRDS before drawing
surfaces inside a box, with IHID set to 1, and then call it again after
drawing surfaces inside a box, with IHID set to 0.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDGRDS or c_tdgrds, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrid, tdgtrs, tdinit, tditri, tdlbla,
tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
