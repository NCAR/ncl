.\"
.\"	$Id: gqmk.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQMK 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQMK (Inquire marker type) - retrieves the current value for the marker type.
.SH SYNOPSIS
CALL GQMK (ERRIND, MTYPE)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_marker_type(Gint *err_ind, Gint *marker_type);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP MTYPE 12
(Integer, Output) - Returns the current value for the polymarker type as
set by default or by a call to GSMK.
.RS
.IP "1" 
 . (dot)
.IP "2" 
 + (plus)
.IP "3" 
 * (asterisk) This is the default
.IP "4" 
 o (circle)
.IP "5" 
 X (cross)
.RE
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gpm, gsmk, gsmksc, gscr, gspmci, gqmksc, gqpmci, 
point, points, ginq_marker_type
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
