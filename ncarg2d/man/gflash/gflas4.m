.\"
.\"	$Id: gflas4.m,v 1.1 1993-03-11 16:21:27 haley Exp $
.\"
.TH GFLAS4 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GFLAS4 - A call to GFLAS4 allows you to access a disk file of
plot- ting instructions generated with a GFLAS1 and GFLAS2
sequence in a previous job for use in a GFLAS3 call.
.SH SYNOPSIS
CALL GFLAS4 (IB, FNAME)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gflas4 (int ib, char *fname)
.SH DESCRIPTION 
.IP IB 12
This argument to GFLAS4 is the identifier to be used
for subsequent GFLAS3 calls, and needs to be between 0
and 99 inclusive just as with the argument IB to
GFLAS1.
.IP FNAME 12
The name of the file in which the plotting instructions
are stored; FNAME is a Fortran character variable
specifying the file name of the disk file.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the Fortran 
argument descriptions.
.SH ACCESS
To use GFLAS4 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_gflas4 load 
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
