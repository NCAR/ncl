.\"
.\"	$Id: gqopsg.m,v 1.9 2000-07-11 23:03:13 haley Exp $
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
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
