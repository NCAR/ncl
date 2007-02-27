.\"
.\"	$Id: gqcr.m,v 1.14 2007-02-27 18:20:19 haley Exp $
.\"
.TH GQCR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GQCR (Inquire color representation) - retrieves the color value associated
with a color index on a particular workstation.
.SH SYNOPSIS
CALL GQCR (WKID, CI, TYPE, ERRIND, CR, CG, CB)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void ginq_colr_rep(Gint ws_id, Gint colr_ind, Ginq_type type, Gint *err_ind, Gcolr_rep  *colr_rep);
.SH DESCRIPTION
.IP WKID 12
(Integer, Input) - Gives a workstation identifier as was set in
a previous call to GOPWK.
.IP CI 12
(Integer, Input) - A color index.
.IP TYPE 12
(Integer, Input) - Specifies whether the returned color values are the
ones that were specified by a call to GSCR or whether they are the
color values that are actually being used on the specified workstation
(frequently a workstation will not be able to represent a requested 
color value precisely).
.RS
.IP 0 
Returns the color value set for index CI either by default or by a call to GSCR.
.IP 1 
Returns the color value actually used by the workstation to represent the 
requested color.
.RE
.IP ERRIND 12
(Integer, Output) - If the inquired values cannot be returned correctly,
a non-zero error indicator is returned in ERRIND, otherwise a zero is returned.
Consult "User's Guide for NCAR GKS-0A Graphics" for a description of the
meaning of the error indicators.
.IP CR 12
(Real, Output) - Returns an intensity value for red in the 
range 0. to 1. inclusive.
.IP CG 12
(Real, Output) - Returns an intensity value for green in the 
range 0. to 1. inclusive.
.IP CB 12
(Real, Output) - Returns an intensity value for blue in the 
range 0. to 1. inclusive.
.SH USAGE
For details on setting and using color indices see the man page for GSCR.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library ncarg_gks.
.SH SEE ALSO
Online: 
gscr, gsplci, gsmkci, gstxci, gsfaci, ginq_colr_rep
.sp
Hardcopy: 
User's Guide for NCAR GKS-0A Graphics;
NCAR Graphics Fundamentals, UNIX Version
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
