.\"
.\"	$Id: gdeactivate_ws.m,v 1.16 2008-12-23 00:03:04 haley Exp $
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
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
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
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
