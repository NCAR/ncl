.\"
.\"	$Id: gclose_ws.m,v 1.1 1993-03-21 01:29:15 haley Exp $
.\"
.TH GCLOSE_WS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gclose_ws (Close workstation) - closes a workstation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gclose_ws(Gint ws_id);
.SH DESCRIPTION
.IP ws_id 12
(Input) - A number identifying the workstation to be
closed.  ws_id must be the same as that used in some previous gopen_ws call.
.SH USAGE
Close workstation updates the workstation and removes its identifier
from the set of open workstations.  The connection to the workstation
is released and the associated connection ID is available for re-use.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gopen_gks(3NCARG),
.BR gopen_ws(3NCARG),
.BR gactivate_ws(3NCARG),
.BR gdeactivate_ws(3NCARG),
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
