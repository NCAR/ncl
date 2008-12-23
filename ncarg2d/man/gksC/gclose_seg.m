.\"
.\"	$Id: gclose_seg.m,v 1.16 2008-12-23 00:03:04 haley Exp $
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
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
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
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
