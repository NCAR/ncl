.\"
.\"	$Id: gcrsg.m,v 1.16 2008-12-23 00:03:02 haley Exp $
.\"
.TH GCRSG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCRSG (Create segment) - creates a GKS segment.
.SH SYNOPSIS
CALL GCRSG(SGNA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gcreate_seg(Gint seg_name);
.SH DESCRIPTION
.IP SGNA 12
(Integer, Input) - Specifies the segment identifier that will be used in
calls to subsequent segment functions applying to this segment.  There can be
a maximum of 100 segments in a given job step.  Legal values for SGNA are
integers between 0 and 99 inclusive.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gclsg, gcsgwk, gdsg, gqopsg, gqsgus, gssgt., gcreate_seg
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
