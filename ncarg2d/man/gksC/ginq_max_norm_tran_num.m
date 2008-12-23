.\"
.\"	$Id: ginq_max_norm_tran_num.m,v 1.16 2008-12-23 00:03:04 haley Exp $
.\"
.TH GINQ_MAX_NORM_TRAN_NUM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_max_norm_tran_num (Inquire maximum normalization transformation number) - retrieves 
the maximum number of supported normalization transformations.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_max_norm_tran_num(Gint *err_ind, Gint *max_norm_tran_num);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP max_norm_tran_num 12
(Output) - Gives the maximum number of normalization transformations 
available in the GKS package. In NCAR GKS-0A, max_norm_tran_num = 2.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gsel_norm_tran(3NCARG),
.BR ginq_cur_norm_tran_num(3NCARG),
.BR ginq_norm_tran(3NCARG),
.BR getset(3NCARG),
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
