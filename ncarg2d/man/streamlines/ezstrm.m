.TH EZSTRM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EZSTRM - 
Used as a front-end to STRMLN with a simpler interface.
Like STRMLN, it may be used to create a streamline field
flow plot in a single call. The restrictions are: (1) the
actual first dimension of the array must be equal to the
size given to Streamlines (i.e. IMAX); and (2), assuming
the default value of the compatibility mode parameter, CPM,
Streamlines will perform a SET call and will draw a
perimeter around the plot using a call to PERIM.
.SH SYNOPSIS
CALL EZSTRM (U,V,WORK,IMAX,JMAX) 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ezstrm (float *u, float *v, float *work, int imax, int jmax)
.SH DESCRIPTION 
.IP U 12
(REAL 2-dimensional array, dimensioned IMAX x n: n >=
JMAX, input) By default, assumed to contain the first
dimensional Cartesian components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector magnitudes.
.IP V 12
(REAL 2-dimensional array, dimensioned IMAX x n: n >=
JMAX, input) By default, assumed to contain the second
dimensional Cartesian components of the vector field.
However, if PLR is non-zero, it is treated as containing
the vector angles.
.IP WORK 12
(REAL array, dimensioned n: n>= 2*IMAX*JMAX working
space): User provided work array used to store the
normalized vector component values, and also to keep track
of the grid boxes eligible for starting a streamline or
placement of a directional arrow.
.IP IMAX 12
(INTEGER, input) Actual size of the first dimension of
arrays U and V
.IP JMAX 12
(INTEGER, input) Assumed size of the second dimension
of arrays U and V.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions with the following exceptions:
.sp
.IP imax 12
The second dimension of u and v in the calling program.
.IP jmax 12
The first dimension of u and v in the calling program.
.SH ACCESS
To use EZSTRM, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
streamlines,
fx,
fy,
stgeti,
stgetr,
stinit,
stream,
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
