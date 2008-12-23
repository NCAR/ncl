.\"
.\"	$Id: gcopy_seg_ws.m,v 1.16 2008-12-23 00:03:04 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
