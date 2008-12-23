.\"
.\"	$Id: gdsg.m,v 1.16 2008-12-23 00:03:02 haley Exp $
.\"
.TH GDSG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GDSG (Delete segment) - deletes a GKS segment.
.SH SYNOPSIS
CALL GDSG(SGNA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gdel_seg(Gint seg_name);
.SH DESCRIPTION
.IP SGNA 12
(Integer, Input) - Specifies the segment identifier for a segment that
will be deleted by a call to this function.  
.SH USAGE
The segment name is removed from the set of names of segments
that are currently in use.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gclsg, gcrsg, gcsgwk, gqopsg, gqsgus, gssgt., gdel_seg
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
