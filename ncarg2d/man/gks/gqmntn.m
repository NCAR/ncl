.\"
.\"	$Id: gqmntn.m,v 1.14 2007-02-27 18:20:20 haley Exp $
.\"
.TH GQMNTN 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQMNTN (Inquire maximum normalization transformation number) - retrieves 
the maximum number of supported normalization transformations.
.SH SYNOPSIS
CALL GQMNTN (ERRIND, MAXTRN)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_max_norm_tran_num(Gint *err_ind, Gint *max_norm_tran_num);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP MAXTRN 12
(Integer, Output) - 
Gives the maximum number of normalization transformations 
available in the GKS package. In NCAR GKS-0A, MAXTRN = 2.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gselnt, gqcntn, gqnt, getset, ginq_max_norm_tran_num
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
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
