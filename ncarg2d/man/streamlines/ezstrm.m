.\"
.\"	$Id: ezstrm.m,v 1.1 1993-03-11 16:33:27 haley Exp $
.\"
.TH EZSTRM 3NCARG "12 June 1991" UNIX "NCAR GRAPHICS"
.SH NAME
EZSTRM - draws a streamline representation of the flow field.
The representation is independent of the flow speed.  EZSTRM
requires that the whole array is to be processed, the arrays
are dimensioned U(IMAX,JMAX), V(IMAX,JMAX), and
WORK(2*IMAX*JMAX).   The window and viewport are chosen by
STRMLN, and PERIM is called.
.SH SYNOPSIS
CALL EZSTRM (U,V,WORK,IMAX,JMAX)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ezstrm (float *u, float *v, float *work, int imax, int jmax)
.SH DESCRIPTION 
.IP U,V 12
Two dimensional arrays containing the velocity fields
to be plotted.  Note: If the U and V components are,
for example, defined in Cartesian coordinates and the
user wishes to plot them on a different projection
(such as stereographic), then the appropriate
transformation must be made to the U and V components
via the functions FU and FV (located in DRWSTR).
.IP WORK 12
User provided work array. The dimension of this array
must be .GE. 2*IMAX*JPTSY.
.sp
Caution: This routine does not check the size of the
work array.
.IP IMAX 12
The first dimension of U and V in the calling program
(X-direction).
.IP JMAX 12
The second dimension of U and V in the calling program
(Y-direction).
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions with the following exceptions:
.sp
.IP imax 12
The second dimension of u and v in the calling program.
.IP jmax 12
The first dimension of u and v in the calling program.
.SH ACCESS
To use EZSTRM load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_ezstrm load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
strmln, ezstrm, ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
