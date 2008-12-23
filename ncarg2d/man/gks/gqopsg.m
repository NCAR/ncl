.\"
.\"	$Id: gqopsg.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQOPSG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQOPSG (Inquire name of open segment) - returns the name of the currently
open segment.
.SH SYNOPSIS
CALL GQOPSG(ERRIND,SGNA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_name_open_seg(Gint *err_ind, Gint *name_open_seg);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the GKS state is SGOP (segment open), 
then ERRIND is returned as "0"; otherwise ERRIND is returned as "4".
.IP SGNA 12
(Integer, Output) - If the GKS state is SGOP (segment open), then 
SGNA is returned as the number of the currently open segment; otherwise
SGNA is undefined.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gclsg, gcrsg, gcsgwk, gdsg, gqsgus, gssgt., ginq_name_open_seg
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
