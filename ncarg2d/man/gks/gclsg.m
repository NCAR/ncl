.\"
.\"	$Id: gclsg.m,v 1.1 1993-03-11 16:21:51 haley Exp $
.\"
.TH GCLSG 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GCLSG (Close segment) - closes a GKS segment.
.SH SYNOPSIS
CALL GCLSG
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
gevtm, gactm, gcrsg, gcsgwk, gdsg, gqopsg, gqsgus, gssgt.
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
