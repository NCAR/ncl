.TH CPRECT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPRECT - Initializes the contouring of a rectangular array
of data.
.SH SYNOPSIS
CALL CPRECT (ZDAT, KZDT, MZDT, NZDT, RWRK, LRWK, IWRK, LIWK) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cprect (float *zdat, int kzdt, int mzdt, int nzdt, \\
.br
float *rwrk, int lrwk, int *iwrk, int liwk) 
.SH DESCRIPTION 
.IP ZDAT 12
(REAL array, dimensioned KZDT x n, where "n" is 
greater than or equal to NZDT, input) is the array of data 
to be contoured.
.IP KZDT 12
(INTEGER, input) is the first dimension of the array 
ZDAT.
.IP MZDT 12
(INTEGER, input) is the first dimension of the array 
of data in ZDAT. MZDT must be less than or equal to KZDT.
.IP NZDT 12
(INTEGER, input) is the second dimension of the array 
of data in ZDAT. NZDT must be less than or equal to the 
declared second dimension of the array ZDAT.
.IP RWRK 12
(REAL array, dimensioned LRWK, input/output) is the 
real work array.
.IP LRWK 12
(INTEGER, input) is the length of RWRK.
.IP IWRK 12
(INTEGER array, dimensioned LIWK, input/output) is the 
integer work array.
.IP LIWK 12
(INTEGER, input) is the length of IWRK.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exceptions:
.IP "zdat(l,kzdt)" 12
zdat is dimensioned l by kzdt, where l \(>= nzdt.
.IP "kzdt" 12
The second dimension of the array zdat. 
.IP "mzdt" 12
The second dimension of the array of data in zdat. mzdt \(<= kzdt. 
.IP "nzdt" 12
The first dimension of the array of data in zdat. nzdt \(<= l, 
where l is the declared first dimension of the array zdat. 
.SH USAGE@@@
CPRECT initializes the internal pointers that are used to manage
workspace use and decides what the ranges of X and Y coordinates
used to draw contour lines and position labels ought to be. 
<<< confusing, long sentence: how about "CPRECT initializes the
internal pointers that manage workspace use; also, it decides the
appropriate ranges of X and Y coordinates for drawing contour
lines and positioning labels. <<<
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
cpex01,
cpex02,
cpex03,
cpex04,
cpex05,
cpex06,
cpex07,
cpex08,
cbex01,
tconpa.
.SH ACCESS
To use CPRECT, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_cprect, 
load the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cppkcl, cppklb, cprset, cpscae, cpsetc, cpseti,
cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
Tutorial: A Step-by-Step Guide to Contouring and Mapping
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

