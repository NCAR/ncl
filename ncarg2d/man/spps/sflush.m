.TH SFLUSH 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
SFLUSH - Flushes polylines, accumulated through calls to the routines
PLOTIF and PLOTIT, from the SPPS polyline buffer shared by those routines;
updates all open workstations; and flushes all system-level I/O buffers.
.SH STATUS
SFLUSH was called FLUSH in previous versions.  The name was changed
to avoid a naming conflict with a common system routine.
.SH SYNOPSIS
CALL SFLUSH
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_sflush() 
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: 
(none)
.SH ACCESS
To use SFLUSH or c_sflush, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
ngpict, frame, plotif, spps, spps_params, ncarg_cbind
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
