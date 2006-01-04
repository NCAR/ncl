.\"
.\"	$Id: gopen_gks.m,v 1.13 2006-01-04 00:13:05 haley Exp $
.\"
.TH GOPEN_GKS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gopen_gks (Open GKS) - opens the GKS package
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gopen_gks(const char *err_file, size_t mem_unit);
.SH DESCRIPTION
.IP err_file 12
(Input) - The place to which error messages are to be written. 
Typically this should be "stdout".
.IP mem_unit 12
(Input) - The dimension of an internal buffer. 
Currently in NCAR GKS-0A, mem_unit is ignored.
.SH USAGE
To get output to any workstation,
gopen_gks, gopen_ws and gactivate_ws must be called in that order.
For jobs producing only CGM output, the three calls to gopen_gks, gopen_ws,
and gactivate_ws 
can be replaced with a call to the SPPS function c_opngks().
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gopen_ws(3NCARG),
.BR gactivate_ws(3NCARG),
.BR gdeactivate_ws(3NCARG),
.BR gclose_ws(3NCARG),
.BR gclose_gks(3NCARG),
.BR gupd_ws(3NCARG),
.BR opngks(3NCARG),
.BR clsgks(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2006
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
