.TH EZHFTN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
EZHFTN - draws a half-tone
picture from data stored in a rectangular array with the
intensity in the picture proportional to the data value.
.SH UTILITY
This routine is part of the Halftone utility in NCAR Graphics. To see
the overview man page for this utility, type "man halftone".
.SH STATUS
EZHFTN is obsolete.  It has been replaced by the CPCICA entry of the
Conpack contouring package.
.sp
EZHFTN continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use CPCICA.
.SH SYNOPSIS
CALL EZHFTN (Z,M,N)
.SH DESCRIPTION 
.IP Z 12
(an input array of type REAL) defining a two-dimensional field to be
half-tone plotted.
.IP M 12
(an input parameter of type INTEGER) which is the first dimension of
the Z array.
.IP N 12
(an input parameter of type INTEGER) which is the second dimension of
the Z array.
.SH USAGE
EZHFTN has a shortened argument list from entry HAFTON which is
based upon the following assumptions.
.nf

1.  The entire Z array is to be plotted.
2.  The lowest value in Z will be at the lowest intensity of the output device.
3.  The highest value in Z will be at the highest intensity of the output device.
4.  Other values in Z will appear linearly spaced.
5.  The maximum number of intensities will be used.
6.  The picture will have a perimeter drawn.
7.  The frame will be advanced after the picture is drawn.
8.  The Z array contains no missing values.

.fi
If these conditions are not met, use the HAFTON entry.
.SH ACCESS 
To use EZHFTN, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
halftone, hafton, halftone_params,
conpack, conpack_params, cpcica
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
