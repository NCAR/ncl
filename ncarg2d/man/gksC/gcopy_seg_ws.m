.\"
.\"	$Id: gcopy_seg_ws.m,v 1.14 2007-02-27 18:20:21 haley Exp $
.\"
.TH GCOPY_SEG_WS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gcopy_seg_ws (Copy segment to workstation) - copies a GKS segment to a GKS 
workstation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gcopy_seg_ws(Gint ws_id, Gint seg_name);
.SH DESCRIPTION
.IP ws_id 12
(Input) - Specifies the workstation identifier (as defined in
a gopen_ws call) that is to receive the segment copy.
.IP seg_name 12
(Input) - Identifies the segment 
that is to be copied to the specified workstation.
seg_name must have been used in a previous call to gcreate_seg.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR geval_tran_matrix(3NCARG),
.BR gaccum_tran_matrix(3NCARG),
.BR gclose_seg(3NCARG),
.BR gcreate_seg(3NCARG),
.BR gdel_seg(3NCARG),
.BR ginq_name_open_seg(3NCARG),
.BR ginq_set_seg_names(3NCARG),
.BR gset_seg_tran(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2007
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
