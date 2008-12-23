.TH TDPLCH 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDPLCH - Draw a character string at a specified position in the plane of the
reference parallelogram.
.SH SYNOPSIS
CALL TDPLCH (XPOS, YPOS, CHRS, SIZE, ANGD, CNTR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdplch(float xpos, float ypos, char *chrs, float size, float angd,
float cntr)
.SH DESCRIPTION
The arguments of TDPLCH are as follows:
.IP "XPOS and YPOS" 8
(input expressions of type REAL) - the parallelogram coordinates of a point
relative to which the character string is to be positioned.
.IP "CHRS" 8
(input, of type CHARACTER) - a string suitable for input to the PLOTCHAR
routine PLCHHQ.
.IP "SIZE" 8
(an input expression of type REAL) - specifies the width of the characters to
be used as a multiple of the "X" dimension of the reference parallelogram. If
the reference parallelogram is not a square, distortion of the characters is
to be expected: for example, if the reference parallelogram is twice as wide
as it is high, then the same will be true of characters drawn by TDPLCH.
.IP "ANGD" 8
(an input expression of type REAL) - specifies the angle, in degrees, at which
the character string is to be drawn relative to the reference parallelogram
(0 => in the direction of positive X, 90 => in the direction of positive Y,
180 => in the direction of negative X, and so on).
.IP "CNTR" 8
(an input expression of type REAL) - specifies how the character string is
to be centered relative to the point (XPOS,YPOS) within the reference
parallelogram.  If CNTR = -1, the leading edge of the character string
will be placed at (XPOS,YPOS); if CNTR = 0, the center of the
character string will be placed at (XPOS,YPOS); and if CNTR = +1,
the trailing edge of the character string will be placed at
(XPOS,YPOS).  Other values may be used, with the logical results; for
example, if CNTR = -.5, the point halfway between the leading edge
and the center of the character string will be placed at (XPOS,YPOS).
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDPLCH or c_tdplch, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdpara, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
