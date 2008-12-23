.\"
.\"	$Id: gschup.m,v 1.16 2008-12-23 00:03:03 haley Exp $
.\"
.TH GSCHUP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
GSCHUP (Set character up vector) - specifies the angle at which subsequent
text is to be drawn with GTX.
.SH SYNOPSIS
CALL GSCHUP (CHUX, CHUY)
.SH C-BINDING SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_char_up_vec(const Gvec *char_up_vec);
.SH DESCRIPTION
.IP CHUX 12
(Real, Input) - 
Gives the X (horizontal) world coordinate of a vector.
.IP CHUY 12
(Real, Input) - 
Gives the Y (vertical) world coordinate of a vector. 
.SH USAGE
The coordinates (CHUX, CHUY) relative to the point (0.,0.) 
establish a vector direction.  This direction specifies the
up direction of individual characters.  This direction also specifies the
orientation of a text string in that the characters in a string
are placed along a line perpendicular to the character up vector.
By default the character up vector is (0.,1.) so that characters
are drawn in their normal horizontal orientation.
The magnitude of the up vector is not used so that (0.,23.) is
the same as (0.,1.).
.SH EXAMPLES
.nf

      CALL GSCHUP(-1.,0.)

.fi
would specify the character up vector as (-1.,0.) and subsequent
text written with GTX would be rotated 90 degrees from normal and
would be appropriate for a Y-axis label.
.sp
.nf

      CALL GSCHUP(1.,1.)

.fi
would specify the character up vector as (1.,1.) and subsequent
text written with GTX would be rotated -45 degress from normal.
.SH ACCESS
To use GKS routines, load the NCAR GKS-0A library 
ncarg_gks.
.SH SEE ALSO
Online: 
gtx, gstxp, gstxal, gstxfp, gschh, gschsp, gschxp, gscr, gstxci, 
gqtxp, gqtxal, gqtxfp, gqchh, gqchsp, gqchup, gqchxp, plotchar, gset_char_up_vec
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
