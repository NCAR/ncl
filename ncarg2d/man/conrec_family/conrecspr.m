.TH CONRECSPR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
conrecspr - a super version of the conrec family.
.TH CONREC_FAMILY 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
conrec_family - is a set of two dimensional contour mapping routines.
There are options for generating standard contours, smoothing contour
lines, labeling the contours, and removing crowded lines.
.SH STATUS
Conrec is obsolete.  It has been replaced by the contouring package,
Conpack.  See the Conpack entry CPCNRC for a simple CONREC replacement.
For more complex examples refer to the conpack and
conpack_params man pages as well as the
NCAR Graphics Contouring and Mapping Tutorial.
.sp
Conrec continues to be provided for compatibility of early NCAR Graphics
codes.  If you are writing new code, we suggest that you use the more
general and rigorous Conpack contouring package.  All Conrec options
are available via Conpack parameters.
.SH SYNOPSIS
EZCNTR(Z,M,N) - draws a contour map based upon a set of default options.
.br
CONREC(Z,L,M,N,FLO,HI,FINC,NSET,NHI,NDOT) - draws a contour map
using options specified as input arguments.
.SH ACCESS 
The Conrec family contains only two user callable entries, EZCNTR, and CONREC.
However, these two entries can be invoked in four different ways to create
contour plots which vary considerably in appearance.  The four variations
include quick, standard, smooth, and super contour lines.  This progression
represents a tradeoff between speed of computation and the appearance of the
contour plots.  These variations are specified through selected command line
options of the ncargf77 command.
.sp
To use EZCNTR or CONREC, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  Other optional libraries
to create the quick, smooth, and super contours will
automatically be linked by the ncargf77 command.
To run a code called mycode.f which has one or more calls to
entry EZCNTR or CONREC, issue one of the commands:
.sp
.IP NORMAL 10
Command:  "ncargf77 mycode.f"
.sp
The contours will be drawn as unsmoothed
dashed or solid lines that can include characters along the lines.
.sp 2
.IP QUICK 10
Command:  "ncargf77 -conrecquick mycode.f"
.sp
The contours will be drawn as unsmoothed
dashed or solid lines without characters along the lines.
.sp 2
.IP SMOOTH 10
Command:  "ncargf77 -conrecsmooth mycode.f"
.sp
The contours will be drawn as smoothed
dashed or solid lines using splines under tension.  There may be
characters along the lines.
.sp 2
.IP SUPER 10
Command:  "ncargf77 -conrecsuper mycode.f"
.sp
The contours will be drawn as smoothed
dashed or solid lines using splines under tension.  There may be
characters along the lines.  Crowded lines can be thinned.
.SH MESSAGES
When error conditions are detected, the support routine SETER 
is called in such a way that it writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates 
execution. The possible error messages are as follows:
.sp
CONREC  - DIMENSION ERROR - M*N .GT. (2**IARTH)
.br
The array to be contoured is dimensioned M by N.  This is larger
than the address space on this computer (2**IARTH) where IARTH is
the size of an address integer.  Check your dimension sizes.
.SH SEE ALSO
Online:
conrec, ezcntr, conrec_params,
conpack, conpack_params, cpcnrc,
dashline_family, ncargf77
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

