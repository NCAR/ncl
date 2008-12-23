.TH GFLAS1 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
GFLAS1 - Initiates storage of plotting
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
(INTEGER, input)
Can be any integer between 0 and 99 inclusive, thus
making it possible to define 100 different storage
buffers in a single job step.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
GFLAS1 automatically assigns the name GNFBxx (GKS New Flash
Buffer) to the file that will receive subsequent plotting
instructions, where xx is the integer value of IB. For example,
if GFLAS1 is called with an argument of 9, then subsequent
plotting instructions will be stored in file GNFB09. You may
need to know the name of the disk file where the plotting
instructions are stored if and when you use GFLAS4. The GNFBxx
files are all created using FORTRAN logical unit IC as
specified in the GOPWK call for WISS.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpmovi,
tgflas,
fgke02.
.SH ACCESS
To use GFLAS1 or c_gflas1, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
gflash,
gflas2,
gflas3,
gflas4,
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
