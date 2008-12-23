.\"
.\"	$Id: gqplci.m,v 1.17 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQPLCI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQPLCI (Inquire polyline color index) - gets the polyline color index.
.SH SYNOPSIS
CALL GQPLCI (ERRIND, COLI)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_line_colr_ind(Gint *err_ind, Gint *line_colr_ind);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP COLI 12
(Integer, Output) - Returns the polyline color index as set by default
of by a call to GSPLCI.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gpl, gsln, gslwsc, gscr, gsplci, gqln, gqlwsc, gqplci,
ginq_line_colr_ind, dashline
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
