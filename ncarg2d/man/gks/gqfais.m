.\"
.\"	$Id: gqfais.m,v 1.4 1993-05-12 17:14:46 haley Exp $
.\"
.TH GQFAIS 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQFAIS (Inquire fill area interior style) - retrieves the current value
for fill area interior style.
.SH SYNOPSIS
CALL GQFAIS (ERRIND, INTS)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_fill_int_style(Gint *err_ind, Gfill_int_style *fill_int_style);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired value cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP INTS 12
(Integer, Output) - Returns the current value for fill style as set by
default or by a call to GSFAIS.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gfa, gsfais, gsfasi, gscr, gsfaci, gqfasi, 
areas, ginq_fill_int_style
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics";
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
