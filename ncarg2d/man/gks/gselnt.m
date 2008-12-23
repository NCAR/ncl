.\"
.\"	$Id: gselnt.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSELNT 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSELNT (Select normalization transformation) - selects a 
predefined or user-defined transformation that maps world coordinates 
to normalized device coordinates.
.SH SYNOPSIS
CALL GSELNT (TRNUM)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gsel_norm_tran(Gint tran_num);
.SH DESCRIPTION
.IP TRNUM 12
(Integer, Input) - A normalization transformation number. The number of 
available transformations is implementation specific. In the case of 
NCAR GKS-0A, two normalization transformations are provided:
.sp
.RS
.IP 0 
Selects the identity transformation in which both the 
window and viewport have the range of 0. to 1. in both 
coordinate directions. This is the default.
.sp
.IP 1 
Selects a normalization transformation in which the 
window and viewport have been defined by calls to GSWN and 
GSVP.
.RE
.SH USAGE
When a normalization transformation is selected, all world coordinate
arguments to GKS functions are transformed by it.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
set, gsup, gswn, gqclip, gsel_norm_tran
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version;
User's Guide for NCAR GKS-0A Graphics
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
