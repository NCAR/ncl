.\"
.\"	$Id: dashdc.m,v 1.1 1993-04-06 19:38:34 haley Exp $
.\"
.TH DASHDC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
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
A character string of arbitrary length (60 characters
seems to be a practical limit) which specifies the dash
pattern to be used. 
<<<Question: What is the dash pattern used for?<<<
A dollar sign in IPAT indicates
solid; an apostrophe indicates a gap; blanks are
ignored. Any character in IPAT which is not a dollar
sign, apostrophe, or blank is considered to be part
of a line label. Each line label can be at most 15
characters in length. Sufficient white space is
reserved in the dashed line for writing line labels.
.IP JCRT 12
Specifies the length in plotter address units per dollar sign or apostrophe.
.IP JSIZE 12
Specifies the size of the plotted characters:
.RS 12
.IP \(bu 4
Between 0 and 3, it is 1., 1.5, 2., and 3. times an 8
plotter address unit width.
.IP \(bu 4
Greater than 3, it is the character width in plotter
address units.
.RE
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use DASHDC, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_dashdc, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
curved, dashline, dashdb, dashdc, frstd, lined, vectd, ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics User's Guide," Version 2.00
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
