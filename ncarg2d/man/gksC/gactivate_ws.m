.\"
.\"	$Id: gactivate_ws.m,v 1.3 1993-05-12 17:16:44 haley Exp $
.\"
.TH GACTIVATE_WS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gactivate_ws (Activate workstation) - 
activates a GKS workstation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gactivate_ws(Gint ws_id);
.SH DESCRIPTION
.IP ws_id 12
(Input) - A number identifying the workstation to be activated.
ws_id must be the same as that used in some previous gopen_ws call.
.SH USAGE
Active workstations receive all GKS output primitives.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR gopen_gks(3NCARG),
.BR gopen_ws(3NCARG),
.BR gdeactivate_ws(3NCARG),
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
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
