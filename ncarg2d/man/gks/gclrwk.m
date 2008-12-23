.\"
.\"	$Id: gclrwk.m,v 1.16 2008-12-23 00:03:02 haley Exp $
.\"
.TH GCLRWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCLRWK (Clear workstation) - clears a workstation.
.SH SYNOPSIS
CALL GCLRWK (WKID, COFL)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gclear_ws(Gint ws_id, Gctrl_flag  ctrl_flag);
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - A number identifying the workstation to be cleared.
WKID must be the same as that used in some previous GOPWK call.
.IP COFL 12
(Integer, Input) - 
Clear the workstation display 
surface. Options are:
.RS
.IP 0 
Check to see if the display surface is empty. If so, do not 
issue a clear surface command.
.IP 1 
Issue a clear surface command whether the surface has been written to or not.
.RE
.SH USAGE
For workstations of type "1" (CGM), GCLRWK is interpreted as a picture 
termination in CGM generation. If COFL = 0 and no output primitives have 
been written in the current picture, then a call to GCLRWK is a "do 
nothing"; otherwise, it generates an END PICTURE element (as well as other 
picture initializing elements). If COFL = 1, then a call to GCLRWK 
generates an END PICTURE.
.sp
For workstations of types "7" or "8" (see the man page for GOPWK for
a description of workstation types) clear workstations erases all
primitives that appear on the screen.
.sp
For all other workstation types, clear workstation simply updates
the workstation.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gopwk, gacwk, gdawk, gclwk, gclear_ws
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
