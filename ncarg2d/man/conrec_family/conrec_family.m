.TH Conrec_family 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Conrec_family - is a set of two dimensional contour mapping routines.
There are options for generating standard contours, smoothing contour
lines, labeling the contours, and removing crowded lines.
.SH STATUS
Conrec_family is obsolete.  It has been replaced by the contouring package,
Conpack.  See the Conpack entry CPCNRC for a simple CONREC replacement.
For more complex examples refer to the conpack and
conpack_params man pages as well as the
NCAR Graphics Contouring and Mapping Tutorial.
.sp
Conrec_family continues to be provided for compatibility of early NCAR
Graphics codes.  If you are writing new code, we suggest that you use the
more general and rigorous Conpack contouring package.  All Conrec_family
options are available via Conpack parameters.
.SH SYNOPSIS
EZCNTR(Z,M,N) - draws a contour map based upon a set of default options.
.br
CONREC(Z,L,M,N,FLO,HI,FINC,NSET,NHI,NDOT) - draws a contour map
using options specified as input arguments.
.SH ACCESS 
The Conrec family contains only two user callable entries, EZCNTR, and CONREC.
However, these two entries can be invoked in four different ways to create
contour plots which vary considerably in appearance.  The four variations
include quick, normal, smooth, and super contour lines.  This progression
represents a tradeoff between speed of computation and the appearance of the
contour plots.  These variations are specified through selected command line
options of the ncargf77 command.
.sp
To use EZCNTR or CONREC, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  Other optional libraries
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
Command:  "ncargf77 -quick mycode.f"
.sp
The contours will be drawn as unsmoothed
dashed or solid lines without characters along the lines.
The QUICK drawing algorithm is faster and cruder than that used for
NORMAL contour lines.  QUICK uses a cell-by-cell analysis rather than
following each contour line to completion in sequence as is done in
the NORMAL algorithm.
.sp 2
.IP SMOOTH 10
Command:  "ncargf77 -smooth mycode.f"
.sp
The contours will be drawn as smoothed
dashed or solid lines using splines under tension.  There may be
characters along the lines.
.sp 2
.IP SUPER 10
Command:  "ncargf77 -super mycode.f"
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
.sp
STLINE  - WARNING FROM ROUTINE STLINE IN CONREC--WORK ARRAY OVERFLOW
.br
This warning message is printed to standard output
if the number of contour lines for a contour level would
exceed the dimension size of work array IR in routine STLINE.
.sp
Should this happen the following messages are also sent
to the output plot file:
.sp
**WARNING--PICTURE INCOMPLETE**
.br
WORK ARRAY OVERFLOW IN STLINE
.sp
Processing stops before the actual array overflow occurs and control
is returned to the calling program.
.SH SEE ALSO
Online:
conrec, ezcntr, conrec_family_params,
conpack, conpack_params, cpcnrc,
dashline_family, ncargf77
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
