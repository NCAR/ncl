.\"
.\"	$Id: gflash.m,v 1.1 1993-03-11 16:21:29 haley Exp $
.\"
.TH GFLASH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GFLASH - Capture and insert specified portions of graphics
instructions in subsequent frames
.SH SYNOPSIS
GFLAS1 - Starts storage of plotting instructions
.br
GFLAS2 - Stops storage of plotting instructions
.br
GFLAS3 - Inserts saved plotting instructions into the output
stream
.br
GFLAS4 - Associates plotting instructions stored in disk file
with id for GFLAS3
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
c_gflas1
.br
c_gflas2
.br
c_gflas3
.br
c_gflas4
.SH ACCESS 
To use GFLASH routines load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_loc, preferably in that order.  To use the
GFLASH  C-bindings load the NCAR Graphics libraries ncargC, ncarg_gksC, 
ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gflash, gflas1, gflas2, gflas3, gflas4, ncarg_cbind
Hardcopy:
"NCAR Graphics Guide to New Utilites, Version 3.00"
.sp
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
