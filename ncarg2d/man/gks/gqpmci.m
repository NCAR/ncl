.\"
.\"	$Id: gqpmci.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQPMCI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQPMCI (Inquire polymarker color index) - retrieves the polymarker color index.
.SH SYNOPSIS
CALL GQPMCI (ERRIND, COLI)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_marker_colr_ind(Gint *err_ind, Gint *marker_colr_ind);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP COLI 12
(Integer, Output) - Returns the polymarker color index as set by default
or by a call to GSPMCI.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gpm, gsmk, gsmksc, gscr, gspmci, gqmk, gqmksc, gqpmci, 
point, points, ginq_marker_colr_ind
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
