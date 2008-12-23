.\"
.\"	$Id: gqsgus.m,v 1.16 2008-12-23 00:03:03 haley Exp $
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
