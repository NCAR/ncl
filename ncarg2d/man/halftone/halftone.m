.TH Halftone 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Halftone - is a utility for drawing half-tone pictures in which the
gray scale level is proportional to the value in a two dimensional array.
.SH STATUS
Halftone is obsolete.  It has been replaced by the CPCICA entry of the
Conpack contouring package.
.sp
Halftone continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use CPCICA.
.SH SYNOPSIS
EZHFTN(Z,M,N) - draws a half-tone picture based upon a set of default options.
.br
HAFTON (Z,L,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPVAL) - Draws a half-tone
picture from data stored in a rectangular array with the
intensity in the picture proportional to the data value.
.SH ACCESS 
To use EZHFTN or HAFTON, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
hafton, ezhftn, halftone_params,
conpack, conpack_params, cpcica
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2006
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
