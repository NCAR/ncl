.TH GFLAS3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GFLAS3 - Inserts the instructions saved on
disk with a previous GFLAS1 identifier IB into the output
stream.  GFLAS3 can be called only after a previous GFLAS1
and GFLAS2 sequence or after a call to GFLAS4. GFLAS3 uses
FORTRAN logical unit IC as specified in the GOPWK call for WISS.
.SH SYNOPSIS
CALL GFLAS3 (IB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gflas3 (int ib)
.SH DESCRIPTION 
.IP IB 12
(INTEGER, input)
The same identifying integer used for an (IB) argument
in GFLAS1.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples:
ccpmovi,
tgflas,
fgke02.
.SH ACCESS
To use GFLAS3 or c_gflas3, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gflash,
gflas1,
gflas2,
gflas4,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
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
