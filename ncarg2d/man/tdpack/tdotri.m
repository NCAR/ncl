.TH TDOTRI 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDOTRI - Order the triangles defined by a triangle list.
.SH SYNOPSIS
CALL TDOTRI (RTRI, MTRI, NTRI, RTWK, ITWK, IORD)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdotri(float *rtri, int mtri, int *ntri, float *rtwk, int *itwk,
int iord)
.SH DESCRIPTION
This routine, given a list of NTRI triangles in the array RTRI and a
real scratch array RTWK of length at least MTRI x 2 , determines the
order in which the triangles are to be rendered and returns a
permutation of the integers from 1 to NTRI in the array ITWK,
defining that permutation.
.sp
The caller may select any of three ways in which the triangles are to be
ordered, the first two of which are essentially identical: When the
argument IORD is given the value 0, the distances of the midpoints of
the triangles from the viewpoint are computed and the triangles are
sorted by decreasing order of these distances. When IORD is given the
value -1, the result is the same, except that the distances of the farthest
points of the triangles from the viewpoint are computed and the
triangles are put in decreasing order of those distances. Both of these
possibilities are appropriate for situations in which the triangles
represent smooth surfaces that do not intersect each other or
themselves; the occasional small errors in the resulting rendering
order should be acceptable.
.sp
If any of the triangles in the list intersect each other or if the surfaces
being depicted are too rough, then the third option should be used:
When IORD is given the value +1, TDOTRI executes an algorithm
taken from the reference "Computer Graphics Principles and
Practice", by Foley and Van Dam. It starts by ordering the triangles as
if IORD had the value -1 (using distances of the far points of the
triangles from the viewpoint), but then it checks for situations in which
this ordering is in error and fixes the errors. Executing this algorithm
can be time-consuming, so it should not be done unless it is really
necessary; one possible way to proceed might be to use IORD = -1 while
checking out a code and then use IORD = +1 only when doing final
plots.
.sp
Sometimes, when IORD = +1, triangles must be broken into smaller
triangles, thereby increasing the total number of triangles in RTRI. If,
as a result of this, NTRI becomes equal to MTRI, no error exit is taken;
instead, TDOTRI just returns control to the caller. Therefore, it's a
good idea, after calling TDOTRI, to check the value of NTRI against
the dimension MTRI; if they're equal, it probably means that the
triangle list filled up and that using the permutation returned in
ITWK will result in an incorrect rendering of the triangles.
.sp
The arguments of TDOTRI are as follows:
.IP "RTRI" 8
(an input/output array, of type REAL, dimensioned 10 x MTRI) -
a list of triangles, probably created by means of calls to TDSTRI, TDITRI,
and/or TDMTRI.  As described above, the number of triangles in the list
may increase as a result of calling TDOTRI.
.IP "MTRI" 8
(an input expression of type INTEGER) - the second dimension of RTRI
and thus the maximum number of triangles the triangle list will hold.
.IP "NTRI" 8
(an input/output variable of type INTEGER) - specifies the number of triangles
currently in the list.  It is the user's responsibility to zero this
initially; its value is increased by each call to a triangle-generating
routine like TDSTRI or TDITRI and may be increased by a call to TDOTRI.
.IP "RTWK" 8
(a scratch array of type REAL, dimensioned at least MTRI x 2).
.IP "ITWK" 8
(an output array, of type INTEGER, dimensioned at least MTRI) -
returned containing a permutation of the integers from 1 to NTRI,
specifying the order in which the triangles ought to be rendered.
.IP "IORD" 8
(an input expression of type INTEGER) - says how the triangles are to
be ordered.  The value 0 implies ordering by decreasing distance of the
triangle midpoints from the eye, -1 implies ordering by decreasing
distance of the triangle farpoints from the eye, and +1 implies ordering
by decreasing distance of the triangle farpoints from the eye, with
adjustments made by running an algorithm from the reference
"Computer Graphics Principles and Practice", by Foley and Van Dam.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDOTRI or c_tdotri, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdpack, tdpack_params, tdpara,
tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdsort, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
