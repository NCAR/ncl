.\"
.\"	$Id: gsvp.m,v 1.1 1993-03-11 16:24:30 haley Exp $
.\"
.TH GSVP 3NCARG "14 January 1992" UNIX "NCAR GRAPHICS"
.SH NAME
GSVP (Set viewport) - establishes a rectangular subspace of 
normalized device coordinates space. Calls to GSVP are 
discouraged with NCAR Graphics. Instead, use the SET 
subroutine because SET also supports mirror imaging and log 
scaling of axes. 
.SH SYNOPSIS
CALL GSVP (TRNUM, XMIN, XMAX, YMIN, YMAX)
.SH DESCRIPTION
.IP TRNUM 12
(Integer, Input) - A normalization transformation 
number. The number of available transformations is implementation 
specific. In the case of NCAR GKS-0A, two normalization 
transformations are provided:
.RS
.IP 0 
Selects the identity transformation in which both the 
window and viewport have the range of 0. to 1. in both 
coordinate directions.  This is the default normalization transformation for 
GKS. It is also fixed within GKS; that is, it is illegal to call GSVP 
with TRNUM = 0.
.IP 1 
A normalization transformation in which the viewport is defined 
by XMIN to XMAX and YMIN to YMAX.
.RE
.IP XMIN 12
(Real, Input) - The left horizontal coordinate of 
the viewport.
.IP XMAX 12
(Real, Input) - The right horizontal coordinate of 
the viewport.
0. \(<= XMIN < XMAX \(<= 1.
.IP YMIN 12
(Real, Input) - The bottom vertical coordinate of 
the viewport.
.IP YMAX 12
(Real, Input) - The top vertical coordinate of the 
viewport.
0. \(<= YMIN < YMAX \(<= 1.
.IP Defaults: 12
TRNUM = 0, XMIN = 0.0, XMAX = 1.0, YMIN = 0.0, YMAX = 1.0
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
set, gswn, gselnt, gqclip
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
(c) Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
