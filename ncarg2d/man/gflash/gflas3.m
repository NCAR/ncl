.\"
.\"	$Id: gflas3.m,v 1.1 1993-03-11 16:21:24 haley Exp $
.\"
.TH GFLAS3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GFLAS3 - A call to GFLAS3 inserts the instructions saved on
disk with a previous GFLAS1 identifier IB into the output
metafile.  GFLAS3 can be called only after a previous GFLAS1
and GFLAS2 sequence or after a call to GFLAS4. GFLAS3 also uses
Fortran logical unit IC for its reads.
.SH SYNOPSIS
CALL GFLAS3 (IB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gflas3 (int ib)
.SH DESCRIPTION 
.IP IB 12
The same identifying integer used for the (IB) argument
in GFLAS1.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the Fortran 
argument description.
.SH ACCESS
To use GFLAS3 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_gflas3 load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gflash, gflas1, gflas2, gflas3, gflas4, ncarg_cbind
.sp
Hardcopy:
"NCAR Graphics Guide to New Utilites, Version 3.00"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
