.TH CPCHCL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
CPCHCL - 
Provides user control as contour lines are
drawn.
.SH SYNOPSIS
CALL CPCHCL (IFLG)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void cpchcl (int *iflg)
.SH DESCRIPTION 
.IP IFLG 12
(INTEGER, input) is +1 if a line is about to be drawn, 
-1 if a line has just been drawn.
.SH USAGE
The routine CPCHCL is not to be called by the user. It is
called by the Conpack routines CPCLDM and CPCLDR just
before and just after the contour lines at each level are
drawn. The default version of CPCHCL does nothing. A
user-supplied replacement may change attributes such as color
and line width (by calling the SPPS routine SETUSV or the
appropriate GKS routines).
.sp
If the element of the parameter array 'CLU' corresponding
to 'PAI' = -1 has been set non-zero to request the drawing
of the edge of the grid, then CPCHCL will be called before
and after that is done. Similarly, if the element of 'CLU'
corresponding to 'PAI' = -2 has been set non-zero, then
CPCHCL will be called before and after the drawing of the
edges of the special-value areas, and, if the element of
\&'CLU' corresponding to 'PAI' = -3 has been set non-zero,
then CPCHCL will be called before and after the drawing of
the edges of the out-of-range areas.
.sp
When CPCHCL is called, the parameter 'PAI' will have been
set to the index of the appropriate contour level (between
1 and 'NCL') or to one of the values -1, -2, or -3. By
retrieving the value of 'PAI', CPCHCL can find out what
line is being drawn; also, a CPGETx call to retrieve an
element of a parameter array like 'CLD' will automatically
get the correct one for the line being drawn.
.SH ACCESS
To use CPCHCL, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use cpchcl from a C
program, load the NCAR Graphics libraries ncargC, ncarg_gksC, 
ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online: 
conpack, 
cpback, cpchcf, cpchhl, cpchil, cpchll, cpcica, cpclam, cpcldm,
cpcldr, cpcltr, cpcnrc, cpdrpl, cpezct, cpgetc, cpgeti, cpgetr, cplbam,
cplbdr, cpmpxy, cppkcl, cppklb, cprect, cprset, cpscae, cpsetc, cpseti,
cpsetr, cpsps1, cpsps2, ncarg_cbind
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved

