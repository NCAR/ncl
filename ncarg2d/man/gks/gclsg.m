.\"
.\"	$Id: gclsg.m,v 1.10 2000-08-22 04:15:56 haley Exp $
.\"
.TH GCLSG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCLSG (Close segment) - closes a GKS segment.
.SH SYNOPSIS
CALL GCLSG
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gclose_seg( void );
.SH USAGE
A call to GCLSG closes the 
currently open segment (graphics output primitives will no longer
be stored in that segment).
GCLSG can be called only if a segment is open.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gcrsg, gcsgwk, gdsg, gqopsg, gqsgus, gssgt, gclose_seg
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
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
