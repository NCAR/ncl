.\"
.\"	$Id: gclsg.m,v 1.4 1993-05-12 17:13:54 haley Exp $
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
