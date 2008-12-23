.\"
.\"	$Id: gqlwsc.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQLWSC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQLWSC (Inquire linewidth scale factor) - retrieves the current setting of the
linewidth scale factor.
.SH SYNOPSIS
CALL GQLWSC (ERRIND, LWIDTH)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_linewidth(Gint *err_ind, Gdouble *linewidth);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP LWIDTH 12
(Real, Output) - Returns the current value of the linewidth scale factor
as set by default or by a call to GSLWSC.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gpl, gsln, gslwsc, gscr, gsplci, gqln, gqplci, dashline, ginq_linewidth
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
