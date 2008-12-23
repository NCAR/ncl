.TH NGMFTC 3NCARG "October 1996" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGMFTC - Temporarily closes a metafile output unit allowing for 
subsequent reopening.
.SH SYNOPSIS
CALL NGMFTC(WKID)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngmftc(int wkid)
.SH DESCRIPTION 
.IP WKID 12
(an input variable of type INTEGER) that specifies the GKS workstation
ID to be temporarily closed.
.SH C-BINDING DESCRIPTION
The C binding argument descriptions are the same as the FORTRAN
argument descriptions.
.SH USAGE
Use NGMFTC to close a metafile temporarily for reopening with
NGREOP.  The temporary close can be done at any time after a
metafile workstation has been opened, even in the middle of drawing
a picture.  The primary use of NGMFTC
is to allow for writing to more than one metafile in a single job
execution.  Since NCAR GKS does not allow for having more than a
single NCGM workstation open at a time, you can use NGMFTC to stop
output to one metafile while you write to another metafile and then
resume output to the initial metafile.
.sp
If you simply want to suspend output to a metafile workstation,
but do not need to write to another metafile workstation, simply
use the GKS entry GDAWK to deactivate the workstation and GOPWK
to reactivate it when you want to resume output.
.sp
For saving the state of all primitive attribute values of an NCGM
workstation at the time of the temporary close and restoring them
at the time of the reopen with NGREOP, see the documenation for
NGSRAT.
.sp
If NGMFTC is used and NGREOP is not subsequently invoked to reopen
the file so that it can be terminated normally in the same job step, 
then an improperly terminated metafile will result from the temporary close.
.SH EXAMPLES
Assuming that an NCGM workstation, with workstation identifier 1, is open,
then
.nf

        CALL NGMFTC(1)

.fi
would temporarily close the workstation.
.sp
Use the ncargex command to see the following relevant example: 
pgkex27.
.SH ACCESS
To use NGMFTC or c_ngmftc, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
The argument to NGMFTC must be an NCGM workstation that is open 
but not active.
.SH SEE ALSO
Online:
ngsrat(3NCARG),
ngreop(3NCARG),
.sp
Online URL:  http://ngwww.ucar.edu/ngdoc/ng/gks/gkshome.html
.sp
Hardcopy:
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
