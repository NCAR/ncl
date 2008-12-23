.TH TDSORT 3NCARG "July 1997" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
TDSORT - A generic sorting routine, normally used indirectly (by calling the
routine TDOTRI).
.SH SYNOPSIS
CALL TDSORT (RWRK, NRWK, IORD, IWRK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_tdsort(float *rwrk, int nwrk, int iord, int *iwrk)
.SH DESCRIPTION
This is an internal routine used for sorting.  Originally, it was not intended
that a user should call this routine directly, but doing so will do no harm.
Given an array of NRWK reals in an array RWRK and an "ordering flag" IORD,
TDSORT returns a permutation vector IWRK such that, for every I and J such
that 1.LE.I.LE.J.LE.NWRK, if IORD is zero, then RWRK(IWRK(I)).LE.RWRK(IWRK(J)),
else RWRK(IWRK(I)).GE.RWRK(IWRK(J)).
.sp
The arguments of TDSORT are as follows:
.IP "RWRK" 8
(an input array, of type REAL, dimensioned NWRK) - an array of NWRK real
numbers that are to be sorted.  The contents of RWRK are returned unchanged;
the ordering is as specified by the contents of the array IWRK.
.IP "NWRK" 8
(an input expression of type INTEGER) - the dimension of RWRK (and of IWRK).
.IP "IORD" 8
(an input expression of type INTEGER) - if IORD is zero, the desired ordering
is from smallest numeric value to largest; otherwise, the desired ordering is
from largest value to smallest.
.IP "IWRK" 8
(an output array of type INTEGER, dimensioned at least NWRK) - returned
containing a permutation of the integers from 1 to NWRK.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use TDSORT or c_tdsort, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order. 
.SH SEE ALSO
Online:
tdclrs, tdctri, tddtri, tdgeti, tdgetr, tdgrds, tdgrid, tdgtrs, tdinit, tditri,
tdlbla, tdlbls, tdline, tdlnpa, tdmtri, tdotri, tdpack, tdpack_params,
tdpara, tdplch, tdprpa, tdprpi, tdprpt, tdseti, tdsetr, tdstri, tdstrs
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
