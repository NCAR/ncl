.TH GFLAS3 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GFLAS3 - Inserts the instructions saved on
disk with a previous GFLAS1 identifier IB into the output
metafile.  GFLAS3 can be called only after a previous GFLAS1
and GFLAS2 sequence or after a call to GFLAS4. GFLAS3 also uses
FORTRAN logical unit IC for its reads.
.SH SYNOPSIS
CALL GFLAS3 (IB)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gflas3 (int ib)
.SH DESCRIPTION 
.IP IB 12
(INTEGER, input)
The same identifying integer used for the (IB) argument
in GFLAS1.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
tgflas.
.SH ACCESS
To use GFLAS3, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_gflas3, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
gflash,
gflas1,
gflas2,
gflas4,
ncarg_cbind.
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
