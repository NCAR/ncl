.TH EZVEC 3NCARG "April 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EZVEC -
A front-end to VELVCT with a simpler interface. Like VELVCT, it plots a
a vector field in a single call.
.SH SYNOPSIS
CALL EZVEC (U,V,M,N)
.SH STATUS
EZVEC is obsolete, and is supported only to provide compatibility
with old NCAR Graphics codes. However, the compatibility mode
parameter, CPM, offers a number of options to help ease the the
transition to the new version of the utility. When writing new code
you are encouraged not to use this entry point, since it provides less
capability than the standard Vectors interface, and may eventually
be phased out.
.SH C-BINDING SYNOPSIS#
#include <ncarg/ncargC.h>
.sp
void c_ezvec (float *u, float *v, int m, int n)
.SH DESCRIPTION
.IP U 12
(REAL 2-dimensional array, dimensioned M x n: n >= N,
input) By default, assumed to contain the first dimensional
Cartesian components of the vector field. However, if PLR
is non-zero, it is treated as containing the vector
magnitudes.
.IP V 12
(REAL 2-dimensional array, dimensioned M x n: n >= N,
input) By default, assumed to contain the second
dimensional Cartesian components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector angles.
.IP M 12
(INTEGER, input) Actual size of the first dimension of
arrays U and V
.IP N 12
(INTEGER, input) Assumed size of the second dimension of
arrays U and V.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions with the following exceptions:
.IP m 12
The actual size of the second dimension of arrays u and v.
.IP n 12
The assumed size of first dimension of arrays u and v.
.SH USAGE
U and V are 2-dimensional vector component arrays, whose actual first
dimensions must be equal to the value of M, and whose second
dimensions must equal or exceed the value of N.
.sp
Assuming the default value of the compatibility mode parameter, CPM,
Vectors always performs a SET call and draws a perimeter around the
plot when accessed through the EZVEC interface. Before the return from
EZVEC, another call to SET restores the previous coordinate system
mapping.  
.sp
By modifying the value of CPM, you may take more control over the
utility than originally possible using this entry point. For instance,
you can override the default value of the SET parameter, or use the
Version 3.2 coordinate system mapping routines instead of the old FX,
FY, MXF, and MYF functions. Nevertheless, when creating new code, use
the VVINIT/VVECTR interface, since its capabilities are greater and
more likely to improve with time.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
tvelvc.
.SH ACCESS
To use EZVEC, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
vectors,
vectors_params,
fx,
velvct,
vvectr,
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br

The use of this Software is governed by a License Agreement.
