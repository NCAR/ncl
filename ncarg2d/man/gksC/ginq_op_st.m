.\"
.\"	$Id: ginq_op_st.m,v 1.10 2000-08-22 04:16:10 haley Exp $
.\"
.TH GINQ_OP_ST 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_op_st (Inquire operating state value) - returns the operating state
of GKS. 
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_op_st(Gop_st *op_st);
.SH DESCRIPTION
.IP op_st 12
(Output) - Returns the GKS operating state:
.RS
.IP GST_GKCL
GKS is closed
.IP GST_GKOP
GKS is open
.IP GST_WSOP
Additionally, a workstation is open
.IP GST_WSAC
Additionally, a workstation is active
.IP GST_SGOP
Additionally, a segment is open
.RE
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
