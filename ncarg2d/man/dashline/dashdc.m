.TH DASHDC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
DASHDC - Chooses a dash pattern with labels.
.SH SYNOPSIS
CALL DASHDC (IPAT, JCRT, JSIZE)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_dashdc (char *ipat, int jcrt, int jsize)
.SH DESCRIPTION 
.IP IPAT 12
(an input parameter of type CHARACTER) the value of which is
the dash pattern to be used.  Although IPAT is of arbitrary
length, 60 characters seems to be a practical limit.
This pattern is repeated for successive
line segments until the full line is drawn.
A dollar sign in IPAT indicates
solid; an apostrophe indicates a gap; blanks are
ignored. Any character in IPAT which is not a dollar
sign, apostrophe, or blank is considered to be part
of a line label. Each line label can be at most 15
characters in length. Sufficient white space is
reserved in the dashed line for writing line labels.
.IP JCRT 12
(an input parameter of type INTEGER) the value of which is
the length in Normalized Device Coordinates (NDCs) * 1024 to be
assigned to each increment of the line pattern.  Increments are
either a gap (dollar sign) or a line segment (apostrophe).
JCRT must be greater than or equal to 1 to produce a line draw.
.IP JSIZE 12
(an input parameter of type INTEGER) the value of which is
the width of the plotted characters according to:
.nf

  JSIZE    NDCs
    0     \.0078
    1     \.0117
    2     \.0156
    3     \.0234

JSIZE > 3, gives the character width as JSIZE/1024. NDCs.
.fi
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
spset1,
spset2,
tdashc, tdashp, tdashs, 
fdldashc, fdldashd. 
.SH ACCESS
To use DASHDC, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_dashdc, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
dashline, dashline_params,
curved, dashdb, frstd, lastd, lined, reset, vectd, ncarg_cbind
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
