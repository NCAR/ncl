.TH FRAME 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
FRAME - advances to the next picture (in the case of CGM output), or
pauses in the window of most recent creation (for X11 output).
A mouse or key click in the window on pause will cause all
active workstations to be cleared.
.SH SYNOPSIS
CALL FRAME
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_frame()
.SH USAGE
If a CGM workstation is open, a call to routine FRAME causes
an END_PICTURE CGM element to be generated.
If there are open X windows, it updates all
workstations and pauses in the window of most recent creation.
After a mouse click or a key click in the window on pause, all
workstations are cleared.
.sp
FRAME is most applicable to an environment with a single active
workstation.  When multiple workstations are used, the more
versatile NGPICT routine should be considered.  NGPICT includes
the option of issuing a "<READY>" prompt
when a window is on pause.
.sp
To use NGPICT to replace routine FRAME for a single CGM workstation use:
CALL NGPICT(WKID,1),
where WKID is the workstation ID for the metafile and the
second argument flags a CLEAR WORKSTATION.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
agex06
.SH ACCESS
To use FRAME or c_frame, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
sflush, ngpict, spps, spps_params, ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
