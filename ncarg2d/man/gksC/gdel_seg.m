.\"
.\"	$Id: gdel_seg.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GDEL_SEG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gdel_seg (Delete segment) - deletes a GKS segment.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gdel_seg(Gint seg_name);
.SH DESCRIPTION
.IP seg_name 12
(Input) - Specifies the segment identifier for a segment that
will be deleted by a call to this function.  
.SH USAGE
The segment name is removed from the set of names of segments
that are currently in use.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR geval_tran_matrix(3NCARG),
.BR gaccum_tran_matrix(3NCARG),
.BR gclose_seg(3NCARG),
.BR gcreate_seg(3NCARG),
.BR gcopy_seg_ws(3NCARG),
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
