.TH Gflash 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Gflash - Allows for storing segments of a picture for insertion
into any subsequent picture.
.SH SYNOPSIS
GFLAS1 - Starts storage of plotting instructions.
.sp
GFLAS2 - Stops storage of plotting instructions.
.sp
GFLAS3 - Inserts saved plotting instructions into the output
stream.
.sp
GFLAS4 - Associates the filename of a previously generated picture segment
with an id suitable for use with GFLAS3.
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
To use Gflash C or Fortran routines, load the NCAR Graphics libraries ncarg,
ncarg_gks, and ncarg_c, preferably in that order.
.SH MESSAGES
When error conditions are detected, the support routine SETER
is called in such a way that it writes a message to the standard
error file (as defined by I1MACH(4)) and then terminates
execution. The possible error messages are as follows:
.IP "GFLAS1 CALLED CONSECUTIVELY WITHOUT AN INTERVENING GFLAS2 CALL"
You must terminate the usage of one FLASH buffer before
reusing it.
.IP "THE NON-NCAR GKS PACKAGE IS NOT LEVEL 2"
Gflash requires GKS level 2A or higher in order to function in a
non-NCAR GKS environment.
.IP "WISS MUST BE OPEN BEFORE CALL TO GFLAS1"
Gflash entries must have access to the appropriate GKS segmentation
functions in order to work.  This is accomplished by making a call to
the GKS entry GOPWK to open WISS.
.IP "MAXIMUM NUMBER OF OPEN WORKSTATIONS ALLOWED IS 100"
You have called GFLAS1 with more than 100 different
identifiers.
.IP "BUFFER IDENTIFIER .LT. 0, OR .GT. 99"
The argument to Gflash is out of the described range.
.IP "GKS MUST BE OPEN BEFORE A CALL TO GFLAS1"
You have tried calling GFLAS1 without opening GKS.  A call to the GKS
entry GOPWK is required before calling any Gflash entry.
.IP "GFLAS2 CALLED WITHOUT CALLING GFLAS1"
GFLAS2 terminates the GFLAS1 job of writing plotting 
instructions to disk.  GFLAS2
has no functionality if not called in the legal sequence.
.IP "WISS NOT OPEN AT GFLAS2 TIME"
Gflash entries must have access to the appropriate GKS
segmentation functions in order to work.  This is 
accomplished by making a call
to the GKS entry GOPWK to open WISS.
.IP "GFLAS3 CALLED WITHOUT CALLING GFLAS2 OR GFLAS4"
GFLAS3 has no access to the plotting instructions that it is
to insert into the output metafile unless a call to either
GFLAS2 or GFLAS4 is made first.
.IP "THE NON-NCAR GKS PACKAGE IS NOT LEVEL 2"
Gflash requires GKS level 2A or higher in order to function
in a non-NCAR GKS environment.
.IP "WISS MUST BE OPEN BEFORE A CALL TO GFLAS4"
Gflash entries must have access to the appropriate GKS
segmentation functions in order to work.  This is accomplished by making a
call to the GKS entry GOPWK to open WISS.
.IP "MAXIMUM NUMBER OF WORKSTATIONS ALLOWED IS 100"
You have called GFLAS1 with more than 100 different
identifiers.
.IP "GKS MUST BE OPEN BEFORE A CALL TO GFLAS4"
You have tried calling GFLAS4 without opening the NCAR GKS
package.  (The GFLAS4 subroutine is an option only 
if you are using the NCAR GKS
package.)
.IP "CANNOT BE CALLED WHILE GFLAS1 IS STILL OPEN"
Call GFLAS2 to complete the GFLAS1/GFLAS2 sequence 
before attempting to call GFLAS4.
.SH SEE ALSO
Online:
gflas1,
gflas2,
gflas3,
gflas4,
ncarg_cbind.
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
