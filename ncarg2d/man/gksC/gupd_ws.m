.\"
.\"	$Id: gupd_ws.m,v 1.11 2000-07-11 23:03:36 haley Exp $
.\"
.TH GUPD_WS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gupd_ws (Update workstation) - Insures that the workstation is current and
reflects all requests that have been made.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gupd_ws(Gint ws_id, Gupd_regen_flag upd_regen_flag);
.SH DESCRIPTION
.IP ws_id 12
(Input) A number identifying the workstation to be updated.
ws_id must be the same as that used in some previous gopen_ws call.
.IP upd_regen_flag 12
(Input) A flag to specify if the current picture should be regenerated.  The 
possible values include:
.RS
.IP GUPD_NOT_PEND 12
Postpone
.IP GUPD_PEND
Perform
.RE
.sp
For the workstation types supported in NCAR GKS, this flag should always be 
set to GUPD_PEND.
.SH USAGE
A call to gupd_ws flushes all buffers to bring the workstation up to date.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks, and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR sflush(3NCARG),
.BR plotif(3NCARG), 
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
