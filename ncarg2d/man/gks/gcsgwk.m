.\"
.\"	$Id: gcsgwk.m,v 1.16 2008-12-23 00:03:02 haley Exp $
.\"
.TH GCSGWK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCSGWK (Copy segment to workstation) - copies a GKS segment to a GKS 
workstation.
.SH SYNOPSIS
CALL GCSGWK(WKID,SGNA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gcopy_seg_ws(Gint ws_id, Gint seg_name);
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - Specifies the workstation identifier (as defined in
a GOPWK call) that is to receive the segment copy.
.IP SGNA 12
(Integer, Input) - Identifies the segment 
that is to be copied to the specified workstation.
SGNA must have been used in a previous call to GCRSG.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gclsg, gcrsg, gdsg, gqopsg, gqsgus, gssgt., gcopy_seg_ws
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
