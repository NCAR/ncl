.\"
.\"	$Id: gclear_ws.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GCLEAR_WS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gclear_ws (Clear workstation) - clears a workstation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gclear_ws(Gint ws_id, Gctrl_flag  ctrl_flag);
.SH DESCRIPTION
.IP ws_id 12
(Input) - A number identifying the workstation to be cleared.
ws_id must be the same as that used in some previous gopen_ws call.
.IP ctrl_flag 12
(Input) - 
Clear the workstation display 
surface. Options are:
.RS
.IP GFLAG_COND
Check to see if the display surface is empty. If so, do not 
issue a clear surface command.
.IP GFLAG_ALWAYS
Issue a clear surface command whether the surface has been written to or not.
.RE
.SH USAGE
For workstations of type "1" (CGM), gclear_ws is interpreted as a picture 
termination in CGM generation. If ctrl_flag = GFLAG_COND and no output 
primitives have been written in the current picture, then a call to gclear_ws 
is a "do nothing"; otherwise, it generates an END PICTURE element (as well as 
other picture initializing elements). If ctrl_flag = GFLAG_ALWAYS, then a 
call to gclear_ws generates an END PICTURE.
.sp
For workstations of types "7" or "8" (see the man page for gopen_ws for
a description of workstation types) clear workstations erases all
primitives that appear on the screen.
.sp
For all other workstation types, clear workstation simply updates
the workstation.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gopen_ws(3NCARG),
.BR gactivate_ws(3NCARG),
.BR gdeactivate_ws(3NCARG),
.BR gclose_ws(3NCARG),
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
