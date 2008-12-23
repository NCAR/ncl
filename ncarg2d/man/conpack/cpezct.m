.TH CPEZCT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPEZCT - Draws black and white contours with a single call.
Simulates the behavior of the old subroutine EZCNTR in
Conrec_family; it has the same arguments and will produce similar
output.
.SH SYNOPSIS
CALL CPEZCT (ZDAT,MZDT,NZDT)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cpezct (float *zdat, int mzdt, int nzdt)
.SH DESCRIPTION 
.IP ZDAT 12
(REAL array, dimensioned MZDT x NZDT, input) is the 
array containing the data to be contoured.
.IP MZDT 12
(INTEGER, input) is the first dimension of the FORTRAN 
array ZDAT and of the array of data stored in it.
.IP NZDT 12
(INTEGER, input) is the second dimension of the 
FORTRAN array ZDAT and of the array of data stored in it.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions with the following exceptions:
.IP "z(nzdt,mzdt)" 12
An n by m data array holding values to be contoured.
.IP "mzdt" 12
The second dimension of z.
.IP "nzdt" 12
The first dimension of z.
.SH USAGE
The effect of calling CPEZCT will be exactly the same as if
you had executed the statements:
.sp
CALL CPCNRC (ZDAT,MZDT,MZDT,NZDT,0.,0.,0.,0,0,-682)
.br
CALL FRAME
.sp
See the man page for CPCNRC.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpezct,
cpex09,
tconpa.
.SH ACCESS
To use CPEZCT or c_cpezct, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cppklb, cprect, cprset, cpscae,
cpsetc, cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
