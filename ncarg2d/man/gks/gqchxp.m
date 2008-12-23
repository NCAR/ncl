.\"
.\"	$Id: gqchxp.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQCHXP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCHXP (Inquire character expansion factor) - retrieves the current
value of the character expansion factor.
.SH SYNOPSIS
CALL GQCHXP (ERRIND, CHXP)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_char_expan(Gint *err_ind, Gdouble *char_expan);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP CHXP 12
(Real, Output) - 
Contains the current value of the character expansion factor as set by
default or by a call to GSCHXP.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschsp, gschup, 
gschxp, gscr, gstxci, gqtxp, gqtxal, gqtxfp, gqchh, 
gqchsp, gqchup, plotchar, ginq_char_expan
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
