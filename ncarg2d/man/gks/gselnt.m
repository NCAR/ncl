.\"
.\"	$Id: gselnt.m,v 1.14 2007-02-27 18:20:20 haley Exp $
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
Copyright (C) 1987-2007
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
