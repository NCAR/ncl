.\"
.\"	$Id: gqsgus.m,v 1.12 2005-01-04 15:42:07 haley Exp $
.\"
.TH GQSGUS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQSGUS (Inquire set of segment names in use) - Retrieves what segment
names are currently in use.
.SH SYNOPSIS
CALL GQSGUS(N,ERRIND,OL,SGNA)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_set_seg_names(Gint num_elems_appl_list, Gint start_pos, Gint *err_ind, Gint_list *seg_names, Gint *length_list);
.SH DESCRIPTION
.IP N 12
(Integer, Input) - The set member requested.
.IP ERRIND 12
(Integer, Output) - If the GKS state is WSOP (workstation open), WSAC
(workstation active), or SGOP (segment open), 
then ERRIND is returned as "0"; otherwise ERRIND is returned as "7".
.IP OL 12
(Integer, Output) - The number of segment names that are currently in use.
.IP SGNA 12
(Integer, Output) - If the GKS state is WSOP (workstation open), WSAC
(workstation active), or SGOP (segment open), and if OL is larger than
zero, then SGNA is returned as the name in the Nth element of the list of
segment names that are currently in use; otherwise SGNA is undefined.
.SH USAGE
For NCAR GKS, segment names are integers between 0 and 99 inclusive
(see the man page for GCRSG).  
.sp
If one wants to obtain a list of all 
segment names that are currently
in use, then the procedure usually is first to determines the number of
segment names currently in use by making an initial call to GQSGUS 
with first argument of "1".  If ERRIND is returned as "0", then OL 
will tell you how many segments are currently in use.  
Then, to get the list of all
segment names currently in use, call GQSGUS in a loop on N from 1
to OL and retrieve the N values of SGNA returned from such calls.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gevtm, gactm, gclsg, gcrsg, gcsgwk, gqopsg, gdsg, gssgt., ginq_set_seg_names
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
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
