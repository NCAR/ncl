.\"
.\"	$Id: vect3.m,v 1.1 1993-03-11 16:35:04 haley Exp $
.\"
.TH VECT3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
VECT3 - draws a line between the current pen position and the
point (UA,VA,WA).  The current pen position becomes (UA,VA,WA).
Note that a curve can be drawn by using a FRST3 call followed by
a sequence of VECT3 calls.
.SH SYNOPSIS
CALL VECT3 (UA,VA,WA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_vect3 (float ua, float va, float wa)
.SH DESCRIPTION 
.IP "UA, VA, WA" 12
coordinates where the line should be connected to.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use VECT3 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_vect3 load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
curve3 fence3 frst3 line3 perim3 point3 set3 threed
tick43 vect3 ncarg_cbind
.sp
Hardcopy:  "NCAR Graphics User's Guide, Version 2.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
