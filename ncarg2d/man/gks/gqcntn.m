.\"
.\"	$Id: gqcntn.m,v 1.12 2005-01-04 15:42:06 haley Exp $
.\"
.TH GQCNTN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCNTN (Inquire current normalization transformation number) - retrieves
the number of the current normalization transformation.
.SH SYNOPSIS
CALL GQCNTN (ERRIND, CTNR)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_cur_norm_tran_num(Gint *err_ind, Gint *norm_tran_num);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP CTNR 12
(Integer, Output) - 
Returns the current normalization transformation number as set by default
or by a call to GSELNT.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gselnt, gqmntn, gqnt, getset, ginq_cur_norm_tran_num
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2005
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
