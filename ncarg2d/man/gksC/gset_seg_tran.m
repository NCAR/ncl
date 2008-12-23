.\"
.\"	$Id: gset_seg_tran.m,v 1.17 2008-12-23 00:03:05 haley Exp $
.\"
.TH GSET_SEG_TRAN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_seg_tran (Set segment transformation) - Associates a segment transformation
with a named segment.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_seg_tran(Gint seg_name, const Gtran_matrix tran_matrix);
.SH DESCRIPTION
.IP seg_name 12
(Input) A segment name as used in a previous gcreate_seg call.
.IP tran_matrix 12
(Gfloat, Input) - A 2x3 array giving the GKS segment transformation to
be associated with the segment specified in seg_name.
.SH USAGE
When the segment named in seg_name is displayed, the coordinates of its
primitives are transformed by the matrix specified in tran_matrix.
.sp
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gaccum_tran_matrix(3NCARG),
.BR geval_tran_matrix(3NCARG),
.BR gclose_seg(3NCARG),
.BR gcreate_seg(3NCARG),
.BR gcopy_seg_ws(3NCARG),
.BR gdel_seg(3NCARG),
.BR ginq_name_open_seg(3NCARG),
.BR ginq_set_seg_names(3NCARG),
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
