.\"
.\"	$Id: gclose_gks.m,v 1.11 2000-08-22 04:16:06 haley Exp $
.\"
.TH GCLOSE_GKS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gclose_gks (Close GSK) - closes the GKS package.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gclose_gks( void );
.SH USAGE
A call to gclose_gks insures that the graphics package is properly
terminated.  No graphics calls depending on GKS should be made
during the time that GKS is closed.  If output to a CGM has been
the exclusive requirement of the graphics job, and the SPPS entry c_opngks
has been used to open GKS and to open and activate a CGM workstation,
then c_clsgks can be used to deactivate and close the CGM workstation,
and to close GKS, making a direct call to gclose_gks unnecessary in that
case.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gopen_gks(3NCARG),
.BR gopen_ws(3NCARG),
.BR gactivate_ws(3NCARG),
.BR gdeactivate_ws(3NCARG),
.BR gclose_ws(3NCARG),
.BR gupwk(3NCARG),
.BR opngks(3NCARG),
.BR clsgks(3NCARG),
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
