.\"
.\"	$Id: ginq_cur_norm_tran_num.m,v 1.10 2000-08-22 04:16:08 haley Exp $
.\"
.TH GINQ_CUR_NORM_TRAN_NUM 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_cur_norm_tran_num (Inquire current normalization transformation number) - retrieves
the number of the current normalization transformation.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_cur_norm_tran_num(Gint *err_ind, Gint *norm_tran_num);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP norm_tran_num 12
(Output) - 
Returns the current normalization transformation number as set by default
or by a call to gsel_norm_tran.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gsel_norm_tran(3NCARG),
.BR ginq_max_norm_tran_num(3NCARG),
.BR ginq_norm_tran(3NCARG),
.BR getset(3NCARG),
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
