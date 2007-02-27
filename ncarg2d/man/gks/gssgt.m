.\"
.\"	$Id: gssgt.m,v 1.14 2007-02-27 18:20:21 haley Exp $
.\"
.TH GSSGT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSSGT (Set segment transformation) - Associates a segment transformation
with a named segment.
.SH SYNOPSIS
CALL GSSGT(SGNA,M)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_seg_tran(Gint seg_name, const Gtran_matrix tran_matrix);
.SH DESCRIPTION
.IP SGNA 12
(Integer, Input) - A segment name as used in a previous GCRSG call.
.IP M 12
(Real, Input) - A 2x3 array giving the GKS segment transformation to
be associated with the segment specified in SGNA.
.SH USAGE
When the segment named in SGNA is displayed, the coordinates of its
primitives are transformed by the matrix specified in M.
.sp
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gactm, gevtm, gclsg, gcrsg, gcsgwk, gdsg, gqopsg, gqsgus., gset_seg_tran
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
