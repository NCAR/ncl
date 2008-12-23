.\"
.\"	$Id: ginq_clip.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GINQ_CLIP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
ginq_clip (Inquire clipping indicator) - retrieves the current value of the
clipping indicator as well as the current clipping rectangle.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_clip(Gint *err_ind, Gclip *clip_ind_rect);
.SH DESCRIPTION
.IP err_ind 12
(Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in err_ind, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP clip_ind_rect.clip_ind 12
(Output) - Returns the current value of the clipping 
indicator as set by default or by a call to gset_clip_ind.
.RS
.IP GIND_NO_CLIP
Clipping is off, or deactivated.
.IP GIND_CLIP
Clipping is on. Data outside of the world coordinate window will 
not be plotted.
.RE
.IP clip_ind_rect.clip_rect 12
(Glimit, Output) - Four normalized device coordinates providing the 
corner points of the current clipping rectangle.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR set(3NCARG),
.BR gset_clip_ind(3NCARG),
.BR gset_win(3NCARG),
.BR gsel_norm_tran(3NCARG),
.BR gks(3NCARG),
.BR ncarg_gks_cbind(3NCARG)
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
