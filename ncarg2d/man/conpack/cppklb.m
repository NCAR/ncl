.TH CPPKLB 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPPKLB - Picks a set of labels for labeled contour levels.
.SH SYNOPSIS
CALL CPPKLB (ZDAT, RWRK, IWRK)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_cppklb (float *zdat, float *rwrk, int *iwrk)
.SH DESCRIPTION 
All three arguments are arrays used in the last call to 
CPRECT, CPSPS1, or CPSPS2, the contents of which must not 
have been changed since that call.
.IP ZDAT 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input) is the data array.
.IP RWRK 12
(REAL array, dimensioned as specified in the last call 
to CPRECT, CPSPS1, or CPSPS2, input/output) is the real 
workspace array.
.IP IWRK 12
(INTEGER array, dimensioned as specified in the last 
call to CPRECT, CPSPS1, or CPSPS2, input/output) is the 
integer workspace array.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Normally, CPPKLB is called by Conpack when labels for the
contour levels are needed.  You can call CPPKLB directly (after
the initialization call to CPRECT, CPSPS1 or CPSPS2) when you
want to modify the resulting parameter arrays that specify the
labels.
.sp
If the constant-field-flag 'CFF' is non-zero, indicating that,
during the last call to CPRECT, CPSPS1, or CPSPS2, the data were
found to be essentially constant, CPPKLB does nothing.
Otherwise, CPPKLB examines the first 'NCL' elements of the
parameter array 'CLV', which defines the contour levels, and
the associated parameter arrays, looking for levels that are to
be labeled ('CLU' = 2 or 3) for which no label is specified
(the associated element of 'LLT' is ' ', a single blank.) If any
such levels are found, labels are generated for them.
.sp
The scale factor 'SFU' may be set as a byproduct of choosing
the labels.  See the description of the parameters 'SFU' (scale
factor used) and 'SFS' (scale factor selector) in the 
conpack_params man page.
.sp
After calling CPPKLB, a user
program may examine the generated labels and change them in
various ways.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example:
ccpklb.
.SH ACCESS
To use CPPKLB or c_cppklb, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_c, preferably in that order.  
.SH MESSAGES
See the conpack man page for a description of all Conpack error
messages and/or informational messages.
.SH SEE ALSO
Online:
conpack,
cpback, cpchcf, cpchcl, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cpmviw, cpmvrw, cppkcl, cprect, cprset, cpscae, cpsetc,
cpseti, cpsetr, cpsps1, cpsps2, ncarg_cbind
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial
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

