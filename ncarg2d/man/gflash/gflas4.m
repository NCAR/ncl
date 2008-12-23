.TH GFLAS4 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GFLAS4 - Allows you to access a disk file of
plotting instructions, generated with a GFLAS1 and GFLAS2
sequence in a previous job, for use in a GFLAS3 call.
.SH SYNOPSIS
CALL GFLAS4 (IB, FNAME)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gflas4 (int ib, char *fname)
.SH DESCRIPTION 
.IP IB 12
(INTEGER, input)
Specifies the identifier to be used
for subsequent GFLAS3 calls.  This needs to be between 0
and 99 inclusive, just as with the argument IB to GFLAS1.
.IP FNAME 12
(CHARACTER, input)
Specifies the name of the file in which the plotting instructions
are stored; FNAME is a FORTRAN character variable
specifying the file name of the disk file.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH ACCESS
To use GFLAS4 or c_gflas4, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gflash,
gflas1,
gflas2,
gflas3,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Contouring and Mapping Tutorial;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
