.\"
.\"	$Id: gflas1.m,v 1.1 1993-03-11 16:21:19 haley Exp $
.\"
.TH GFLAS1 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GFLAS1 - A call to GFLAS1 initiates storage of plotting
instructions into a disk file. Instructions subsequent to
GFLAS1, but prior to a GFLAS2 call, will be stored on disk
rather than inserted into the output metafile.
.SH SYNOPSIS
CALL GFLAS1 (IB) 
.sp
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_gflas1 (int ib) 
.SH DESCRIPTION 
.IP IB 12
Can be any integer between 0 and 99 inclusive, thus
making it possible to define 100 different storage
buffers in a single job step.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the Fortran 
argument description.
.SH USAGE
GFLAS1 automatically assigns the name GNFBxx (GKS New Flash
Buffer) to the file that will receive subsequent plotting
instructions, where xx is the integer value of IB. For example,
if GFLAS1 is called with an argument of 9, then subsequent
plotting instructions will be stored in file GNFB09. You may
need to know the name of the disk file where the plotting
instructions are stored if and when you use GFLAS4. The GNFBxx
files are all created using Fortran logical unit IC as
specified in the GOPWK call.
.SH ACCESS
To use GFLAS1 load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use c_gflas1 load 
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
