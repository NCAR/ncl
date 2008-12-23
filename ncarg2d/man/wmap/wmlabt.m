.\"
.\"	$Id: wmlabt.m,v 1.13 2008-12-23 00:03:11 haley Exp $
.\"
.TH WMLABT 3NCARG "January 1995" UNIX "NCAR GRAPHICS"
.SH NAME
WMLABT - plots weather map regional temperatures labels (like 90s, 80s, etc.)
.SH SYNOPSIS
CALL WMLABT (X, Y, LABEL, IPOS)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void c_wmlabt(float x, float y, char *label, int ipos)
.SH DESCRIPTION
.IP X 12
(Real, Input) - An X coordinate (specified in world coordinates) of a
point that specifies a position for the label in LABEL.
.IP Y 12
(Real, Input) - A Y coordinate (specified in world coordinates) of a
point that specifies a position for the label in LABEL.
.IP LABEL 12
(Character, Input) - A character variable that contains the desired label.
.IP IPOS 12
(Integer, Input) - A flag for controlling whether an optional arrow is drawn.
If IPOS is 0, then no arrow is drawn and the label is centered at (X,Y).
If IPOS is between 1 and 12 (inclusive), then an arrow is drawn in one of
twelve positions and (X,Y) specifies the location of the tip of the arrow.
.SH USAGE
Set the values for the appropriate internal parameters before calling
WMLABT to produce the desired label.  The internal parameters that control
the appearance of the labels are: RBS, RFC, RLS, ROS, and THT.  The
appearance of the optional arrows is controlled by the internal parameters
controlling arrows (see the documentation for WMLABS).
.SH ACCESS
To use WMLABT or c_wmlabt, load the NCAR Graphics libraries ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online: 
wmap, wmdflt, wmgetc, wmgeti, wmgetr, wmlabs, wmsetc, wmseti, wmsetr, wmap_params
.sp
Hardcopy: 
WMAP - A Package for Producing Daily Weather Maps and Plotting Station 
Model Data
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
