.TH VVECTR 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
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
void c_vvectr(float *u, float *v, float *p, int *iam, 
.br
              int (*vvudmv_)(float *xcs, float *ycs, 
.br
              int *ncs, int *iai, int *iag, int *nai), 
.br
              float *wrk)
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
(REAL array, dimensioned as specified in last call to VVINIT,
input/output): Work array required only if the parameter VMD is set to
a value greater than 0.0. Otherwise may be set to a dummy value.

.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
A call to VVINIT must precede the first call to VVECTR, and is again
required any time the vector or scalar array data changes, any of the
coordinate control parameters are modified, or the number of color
threshold levels is modified when Vectors is given the responsibility
of setting up the color threshold value array. However, the user may
modify text elements or any of the rendering control parameters, and
if assuming responsibility for setting up the color threshold array,
may also modify the color array parameters, in between multiple
invocations of VVECTR.
.sp
Arguments to VVINIT establish the sizes of the two or three data
arrays, as well as the possible work array. VVINIT places these values
into common block variables where they become available to VVECTR.
Therefore no size arguments need appear in the VVECTR argument
list. When there is no scalar data array, area masking is not enabled,
and VMD is less than or equal to 0.0, all but the first two arguments
to VVECTR may have dummy values and the invocation would be something
like:
.in 15
.sp
CALL VVECTR(U,V,IDM,IDM,IDM,IDM)
.in -15
.PP
where IDM is an arbitrary variable, INTEGER or REAL, that need not
have been initialized.  However, any time the color threshold control
parameter, CTV has an absolute value of 2, the auxiliary scalar data
array is expected, and the P scalar array variable needs to be
specified.  
.sp
The masking capability supported by Vectors allows the user to overlay
vector fields on top of graphics produced by other utilities, such as
Conpack contour plots, without encroaching on areas, like label boxes,
that should appear in the foreground. Normally the area map should be
generated and used in the normal way (as described in the Areas and
Conpack documentation) before calling any routines in the Vectors
utility. When the parameter MSK has a non-zero value, masking is
enabled and a previously created area map must be passed to VVECTR.
VVECTR examines the map, returning an error if it determines that it
is an invalid map or contains more area maps that it can handle,
currently 64. Otherwise its only use of the map is as a member of the
argument list when invoking one of the Areas routines, ARDRLN or
ARGTAI.  The user must also pass in a user-definable masked drawing
subroutine.  A simple version of this subroutine, named VVUDMV, is
supplied with the Vectors utility, and may suffice for basic masked
drawing operations. See the vvudmv man page for more information.
.sp
The last argument in the calling sequence, WRK, is used only when the
parameter VMD (Vector Minimum Distance) is given a value greater than
0.0. In this case, Vectors uses the array to keep track of the
location of each vector in NDC space so that the distances between
vectors can be compared. Based on these comparisons, Vectors
eliminates some vectors such that the remaining vectors are separated
by at least the specified distance.  Otherwise the WRK argument is
ignored, and may be passed a dummy argument value.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples:
bnchmk,
fcover,
ffex00,
ffex01,
ffex02,
ffex05,
stex02,
stex03,
vvex01,
vvex02.
.SH ACCESS
To use VVECTR or c_vvectr, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the vectors man page for a description of all Vectors error
messages and/or informational messages.
.SH SEE ALSO
Online:
vectors,
vectors_params,
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
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
