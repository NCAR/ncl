.TH VVECTR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
VVECTR - 
Manages the coordinate system mapping, color
setting, auxiliary text output, and drawing of the vector
field plot, according to the specifications established by
the parameter setting routines and the initialization
routine, VVINIT.
.SH SYNOPSIS
CALL VVECTR (U,V,P,IAM,VVUDMV,WRK) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vvectr(float *u, float *v, float *p, int *iam, \\
.br
int (*vvudmv_)(float *xcs,
float *ycs, int *ncs, \\
.br
int *iai, int *iag, int *nai), float *wrk)
.SH DESCRIPTION 
.IP U 12
(REAL 2 dimensional array, dimensioned as specified in
last call to VVINIT, input): By default, assumed to contain
the first dimensional axis components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector magnitudes.
.IP V 12
(REAL 2 dimensional array, dimensioned as specified in
last call to VVINIT, input): By default, assumed to contain
the second dimensional axis components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector angles.
.IP P 12
(REAL 2 dimensional array, dimensioned as specified in
last call to VVINIT, input): Array of scalar data that may
be used to color the vectors. The grid points are assumed
to coincide with the grid points of the U and V arrays.
Required only if CTV has an absolute value of 2; otherwise
this argument is ignored and may be assigned a dummy value.
.IP IAM 12
(INTEGER array, unknown dimension, input): Area map
array previously established by calls to routines in the
Areas utility. It is not examined or modified by Vectors.
Required only if MSK is set to a non-zero value; otherwise
it is ignored and may be assigned a dummy value.
.IP VVUDMV 12
(EXTERNAL subroutine, input): User-definable masked
drawing subroutine. See the man page vvudmv 
for a discussion of the usage and argument list
required for this subroutine. Required only if MSK is set
to a non-zero value; otherwise it is ignored and may be
assigned a dummy value.
.IP WRK 12
(REAL array, dimensioned as specified in last call to
VVINIT, input/output): Array intended for future
enhancement of the Vectors utility. It is currently ignored
and may be assigned a dummy value.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
bnchmk,
stex03,
vvex01,
vvex02.
.SH ACCESS
To use VVECTR, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_vvectr, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the vectors man page for a description of all Vectors error
messages and/or informational messages.
.SH SEE ALSO
Online:
vectors,
ezvec,
fx,
fy,
vvgetc,
vvgeti,
vvgetr,
vvinit,
vvrset,
vvsetc,
vvseti,
vvsetr,
vvudmv,
vvumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
