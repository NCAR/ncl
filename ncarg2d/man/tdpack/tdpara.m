.TH TDPARA 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDPARA - Initialization routine for TDPACK, called to set the value of certain
internal values.
.SH SYNOPSIS
CALL TDPARA (UA00, VA00, WA00, UV10, VV10, WV10, UV01, VV01, WV01)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdpara(float ua00, float va00, float wa00, float uv10, float vv10,
float wv10, float uv01, float vv01, float wv01)
.SH DESCRIPTION
This routine may be thought of as an initialization routine or as just a
routine to access certain internal values; it simply transfers the values
of its arguments into TDPACK labelled common blocks for later use by
other TDPACK routines. These values define a "reference
parallelogram".  Each point in the plane in which the parallelogram lies
can be identified by specifying its "parallelogram coordinates": a pair of
reals, one of which may be thought of as an "X" coordinate and the
other of which may be thought of as a "Y" coordinate.
.sp
Calls to TDPARA may be positioned without regard to calls to TDINIT
because the two routines do not affect one another.  Redefining the
reference parallelogram affects the behavior of each of the routines
TDPRPA, TDPRPI, TDGRID, TDLBLA, and TDPLCH, each of which
makes use of parallelogram coordinates in some way.  Be aware that
each of the routines TDGRDS and TDLBLS calls TDPARA to redefine
the reference parallelogram and neither of them restores the original
definition when it is done.
.sp
If a point has "parallelogram coordinates" (XIPA,YIPA), then its actual
3-space coordinates are given by the following equations:
.sp
.nf
  U = UA00+XIPA*UV10+YIPA*UV01
  V = VA00+XIPA*VV10+YIPA*VV01
  W = WA00+XIPA*WV10+YIPA*WV01
.fi
.sp
The point with parallelogram coordinates (0,0) is in what might be
thought of as the "lower left" corner of the parallelogram, while the
point with parallelogram coordinates (1,1) is in what might be thought
of as the "upper right" corner of the parallelogram. Any point of the
plane in which the reference parallelogram lies can be identified using
its parallelogram coordinates, not just the points inside the
parallelogram itself.
.sp
Note that, although the reference parallelogram doesn't have to be
rectangular, a non-rectangular one is probably not very useful; in fact,
a square one defined by unit vectors is probably best, particularly if one
is drawing characters in the plane of the reference parallelogram.  For
example, suppose that you want to write the characters "THE U/V
PLANE" in that part of the U/V plane with U values between 0 and
100 and V values between 0 and 200; it is probably best, in this case, to
use a reference parallelogram with an origin at (0,0,0), an "X" side
with components (1,0,0) and a "Y" side with components (0,1,0).  Then,
in the call to TDPLCH, one can place the character string at
parallelogram coordinates (50,100) and use a character size of 1.5.  If
one used a reference parallelogram with an origin at (0,0,0), an "X"
side with components (100,0,0) and a "Y" side with components
(0,200,0), one could place the character string at parallelogram
coordinates (.5,.5) and use a character size of .015, but the characters
written would be twice as high as they are wide, which is undesirable.
.sp
The arguments of TDPARA are as follows:
.IP "UA00, VA00, and WA00" 8
(input expressions of type REAL) -
the coordinates of the "origin" of the parallelogram: the point with
parallelogram coordinates (0,0).
.IP "UV10, VV10, and WV10" 8
(input expressions of type REAL) -
the U, V, and W components of the vector from the origin of the parallelogram
to the point with parallelogram coordinates (1,0).
.IP "UV01, VV10, and WV01" 8
(input expressions of type REAL) -
the U, V, and W components of the vector from the origin of the parallelogram
to the point with parallelogram coordinates (0,1).
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDPARA or c_tdpara, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
