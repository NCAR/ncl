.TH Isosurface 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Isosurface - Allows the user to draw a
perspective view of the surface defined by the equation
f(u,v,w)=fiso, where the function f is approximated by a 
3-dimensional array of data and fiso is a user-specified
"isovalue". Contours created by taking slices in any of
three directions through the surface are drawn with hidden
portions of the contour lines removed.
.SH SYNOPSIS
EZISOS - May be called to draw an isosurface.
.sp
ISOSRF - May be called to draw an isosurface.
.sp
ISGETI - Retrieves the current values of
various internal parameters affecting the behavior of
ISOSRF.
.sp
ISGETR - Retrieves the current values of
various internal parameters affecting the behavior of
ISOSRF.
.sp
ISSETI - Gives new values to internal parameters.
.sp
ISSETR - Gives new values to internal parameters.
.sp
PWRZI - Plots characters in
three-space when using ISOSRF.
.SH C-BINDING SYNOPSIS
c_ezisos, 
.br
c_isosrf, 
.br
c_isgeti, 
.br
c_isgetr, 
.br
c_isseti, 
.br
c_issetr, 
.br
c_pwrzi 
.SH USER-MODIFIABLE INTERNAL ROUTINES
None.
.SH ACCESS 
To use Isosurface, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH MESSAGES
The error messages described below can be written to the output unit by
routines in the package Isosurface.  All of these errors are fatal and cause
the user's program to terminate execution (on the other hand, all of the
errors are easily avoided).
.IP "ISGETI OR ISGETR - PARAMETER NAME TOO SHORT - x" 12
The argument "PNAM", in a call to ISGETI or ISGETR, is only one character
long.
.IP "ISGETI OR ISGETR - PARAMETER NAME NOT KNOWN - xx" 12
The argument "PNAM", in a call to ISGETI or ISGETR, begins with two
characters that do not constitute one of the legal internal parameter names
described in the man page isosurface_params. 
.IP "ISSETI OR ISSETR - PARAMETER NAME TOO SHORT - x" 12
The argument "PNAM", in a call to ISSETI or ISSETR, is only one character
long.
.IP "ISSETI OR ISSETR - PARAMETER NAME NOT KNOWN - xx" 12
The argument "PNAM", in a call to ISSETI or ISSETR, begins with two
characters that do not constitute one of the legal internal parameter
names described in the man page isosurface_params. 
.IP "ISINIT - INSTALLATION OF ISOSRF IMPROPERLY DONE" 12 
This error message means that the screen models used to do hidden-line
removal have been found to be improperly dimensioned for the system on
which the code is being run. About the only way this can happen is if
the distributed code is run on a a machine with sixteen-bit words or if
the distributed code has been modified incorrectly.
.SH SEE ALSO
Online:
isosurface_params, ezisos, 
isgeti, isgetr, isosrf, isseti, issetr, pwrzi, 
ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
