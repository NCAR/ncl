.\"
.\"	$Id: gqclip.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GQCLIP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCLIP (Inquire clipping indicator) - retrieves the current value of the
clipping indicator as well as the current clipping rectangle.
.SH SYNOPSIS
CALL GQCLIP (ERRIND, ICLIP, CLRECT)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_clip(Gint *err_ind, Gclip *clip_ind_rect);
.SH DESCRIPTION
.IP ERRIND 12
(Integer, Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP ICLIP 12
(Integer, Output) - Returns the current value of the clipping 
indicator as set by default or by a call to GSCLIP.
.RS
.IP 0 
Clipping is off, or deactivated.
.IP 1 
Clipping is on. Data outside of the world coordinate window will 
not be plotted.
.RE
.IP CLRECT 12
(Real, Array(4)) - Four normalized device coordinates providing the 
corner points of the current clipping rectangle in the order 
XMIN, XMAX, YMIN, YMAX.  
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
set, gsclip, gswn, gselnt, ginq_clip
.sp
Hardcopy: 
"User's Guide for NCAR GKS-0A Graphics"
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
