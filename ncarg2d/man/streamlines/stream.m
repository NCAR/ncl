.TH STREAM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
STREAM - 
Outputs the streamline representation of the
flow field, according to the specifications established by
the parameter setting routines and the initialization
routine, STINIT.
.SH SYNOPSIS
CALL STREAM (U,V,P, IAM,STUMSL,WRK) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_stream(float *u, float *v, float *p, int *iam, int (*stumsl_)( float *xcs, float *ycs, int *ncs, int *iai, int *iag, int *nai), float *wrk)
.SH DESCRIPTION 
.IP U 12
(REAL 2 dimensional array, dimensioned as specified in
last call to STINIT, input): By default, assumed to contain
the first dimensional axis components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector magnitudes.
.IP V 12
(REAL 2 dimensional array, dimensioned as specified in
last call to STINIT, input): By default, assumed to contain
the second dimensional axis components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector angles.
.IP P 12
(REAL, input, DUMMY): This argument is currently ignored.
Should be assigned a dummy value.
.IP IAM 12
(INTEGER array, unknown dimension, input): Area map
array previously established by calls to routines in the
Areas utility. It is checked to see that the number of area
groups is within the limits allowed by Streamlines, then
simply passed through to an Areas routine for further
processing. Required only if MSK is set to a non-zero
value; otherwise it is ignored and may be assigned a dummy
value.
.IP STUMSL 12
(EXTERNAL subroutine, input): User-definable masked
drawing subroutine. 
Required only if MSK is set
to a non-zero value; otherwise it is ignored and may be
assigned a dummy value.
.IP WRK 12
(REAL array, dimensioned as specified in last call to
STINIT, input/output): Work array used to store normalized
values of the U and V vector components while the
streamlines are calculated. It also holds bit flags
indicating whether a grid location is eligible for starting
a streamline or placing a directional arrowhead.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
stex01,
stex03.
.SH ACCESS
To use STREAM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_stream, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH MESSAGES
See the streamlines man page for a description of all Streamlines error
messages and/or informational messages.
.SH SEE ALSO
Online:
streamlines,
streamlines_params,
ezstrm,
fx,
fy,
stgeti,
stgetr,
stinit,
strmln,
strset,
stseti,
stsetr,
stuixy,
stumsl,
stumta,
stumxy,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
