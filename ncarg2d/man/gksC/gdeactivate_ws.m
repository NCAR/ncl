.\"
.\"	$Id: gdeactivate_ws.m,v 1.2 1993-05-03 17:29:33 haley Exp $
.\"
.TH GDEACTIVATE_WS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gdeactivate_ws (Deactivate workstation) - deactivates a workstation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gdeactivate_ws(Gint ws_id);
.SH DESCRIPTION
.IP ws_id 12
(Input) - A number identifying the workstation to be deactivated.
ws_id must be the same as that used in some previous gactivate_ws call.
.SH USAGE
The specified workstation is removed from the set of active
workstations.  GKS output primitives are not sent to workstations
that are not active.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gopen_gks(3NCARG),
.BR gopen_ws(3NCARG),
.BR gactivate_ws(3NCARG),
.BR gclose_ws(3NCARG),
.BR gclose_gks(3NCARG),
.BR opngks(3NCARG),
.BR clsgks(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
