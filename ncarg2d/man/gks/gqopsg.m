.\"
.\"	$Id: gqopsg.m,v 1.1 1993-03-11 16:23:12 haley Exp $
.\"
.TH GQOPSG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQOPSG (Inquire name of open segment) - returns the name of the currently
open segment.
.SH SYNOPSIS
CALL GQOPSG(ERRIND,SGNA)
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
gevtm, gactm, gclsg, gcrsg, gcsgwk, gdsg, gqsgus, gssgt.
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
