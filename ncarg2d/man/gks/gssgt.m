.\"
.\"	$Id: gssgt.m,v 1.16 2008-12-23 00:03:03 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
