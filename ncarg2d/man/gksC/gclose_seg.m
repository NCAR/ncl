.\"
.\"	$Id: gclose_seg.m,v 1.1 1993-03-21 01:29:13 haley Exp $
.\"
.TH GCLOSE_SEG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gclose_seg (Close segment) - closes a GKS segment.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gclose_seg( void );
.SH USAGE
A call to gclose_seg closes the 
currently open segment (graphics output primitives will no longer
be stored in that segment).
gclose_seg can be called only if a segment is open.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gksC, ncarg_gks, ncarg_c,
and ncarg_loc libraries.
.SH SEE ALSO
Online: 
.BR geval_tran_matrix(3NCARG),
.BR gaccum_tran_matrix(3NCARG),
.BR gcreate_seg(3NCARG),
.BR gcopy_seg_ws(3NCARG),
.BR gdel_seg(3NCARG),
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
