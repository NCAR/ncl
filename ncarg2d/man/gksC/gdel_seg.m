.\"
.\"	$Id: gdel_seg.m,v 1.1 1993-03-21 01:29:27 haley Exp $
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
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
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
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
