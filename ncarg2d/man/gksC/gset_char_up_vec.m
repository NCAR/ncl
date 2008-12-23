.\"
.\"	$Id: gset_char_up_vec.m,v 1.17 2008-12-23 00:03:04 haley Exp $
.\"
.TH GSET_CHAR_UP_VEC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.SH NAME
gset_char_up_vec (Set character up vector) - specifies the angle at which subsequent
text is to be drawn with gtext.
.SH SYNOPSIS
#include <ncarg/gks.h>
.sp
void gset_char_up_vec(const Gvec *char_up_vec);
.SH DESCRIPTION
.IP char_up_vec.delta_x 12
(Gfloat, Input) - Gives the X (horizontal) world coordinate of a vector.
.IP char_up_vec.delta_y 12
(Gfloat, Input) - Gives the Y (vertical) world coordinate of a vector. 
.SH USAGE
The coordinates (char_up_vec.delta_x, char_up_vec.delta_y) relative to the 
point (0.,0.) 
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

      char_up_vec.delta_x = -1.
      char_up_vec.delta_y = 0.
      gset_char_up_vec(&char_up_vec);

.fi
would specify the character up vector as (-1.,0.) and subsequent
text written with gtext would be rotated 90 degrees from normal and
would be appropriate for a Y-axis label.
.sp
.nf

      char_up_vec.delta_x = 1.
      char_up_vec.delta_y = 1..
      gset_char_up_vec(&char_up_vec);

.fi
would specify the character up vector as (1.,1.) and subsequent
text written with gtext would be rotated -45 degress from normal.
.SH ACCESS
To use the GKS C-binding routines, load the ncarg_gks and
ncarg_c libraries.
.SH SEE ALSO
Online: 
.BR gtext(3NCARG),
.BR gset_text_path(3NCARG),
.BR gset_text_align(3NCARG),
.BR gset_text_font_prec(3NCARG),
.BR gset_char_ht(3NCARG),
.BR gset_char_space(3NCARG),
.BR gset_char_expan(3NCARG),
.BR gset_colr_rep(3NCARG),
.BR gset_text_colr_ind(3NCARG),
.BR ginq_text_path(3NCARG),
.BR ginq_text_align(3NCARG),
.BR ginq_text_font_prec(3NCARG),
.BR ginq_char_ht(3NCARG),
.BR ginq_char_space(3NCARG),
.BR ginq_char_up_vec(3NCARG),
.BR ginq_char_expan(3NCARG),
.BR plotchar(3NCARG),
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
