.TH NGEZLOGO 3NCARG "April 2002" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
NGEZLOGO - Draws various NCAR and UCAR logos, the default being
to draw an NCAR logo of reasonable size in the lower right corner
of a plot.
.SH SYNOPSIS
CALL NGEZLOGO()
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_ngezlogo()
.SH USAGE
Call this subroutine to produce a logo.  By default a
call to this subroutine will draw an NCAR logo of reasonable 
size in the lower right corner of the plot.  This will be
a full-color logo for PostScript output, otherwise it will
be a one-color logo.
.sp
Calls to the ngmisc paramter setting routines NGSETI and
NGSETR can be used for
more control over the ouput.  Calls to these routines can reproduce
the functionality of the subroutine NGLOGO, except that the
requested logo will be plotted to all active workstations, rather
than a specified one.  Here is a list of
control parameters and what they do:
.RS
.IP "'LT'" 
\-  Use with NGSETI to specify the logo type.  See the documentation
for NGLOGO for a list of the logo types, or execute "ncargex miex01"
for an example plot that lists the logo types.  Default is 1.
.IP "'OS'" 
\-  Use with NGSETR to specify the logo size in normalized device
coordinates (a number between 0. and 1.).  For example, a size
specification of 0.1 would give a logo size one-tenth the maximum
plot height.  Default is 0.07.
.IP "'OX'" 
\-  Use with NGSETR to specify the logo X-coordinate position in
normalized device coordinates.  Default is 0.93.
.IP "'OY'" 
\-  Use with NGSETR to specify the logo Y-coordinate position in
normalized device coordinates.  Default is 0.05.
.IP "'LC'" 
\-  Use with NGSETI to specify the logo color (except in the
case of an NCAR logo being plotted to a PostScript workstation,
in which case you will get the full-color logo).  Default is 1.
.IP "'LB'" 
\-  Use with NGSETI to specify the secondary logo color (this is
applicable only for logo type 5, in which case the secondary color
is used for the text string "UCAR").  Default is 1.
.RE
.sp
.SH EXAMPLES
.IP "CALL NGEZLOGO()"
Plots an NCAR logo on all active workstations in the lower right
corner of the plot.  In the case of PostScript output, a full-color
logo will be produced, otherwise a one-color logo will be produced.
.IP "Logo of type 2:"
.RS
CALL NGSETI('LT',2)
.br
CALL NGEZLOGO()
.sp
Plots a UCAR logo (just the star) in the lower right corner of
the plot.
.RE
.sp
.IP "More complete example:"
.RS
CALL NGSETI('LT',5)
.br
CALL NGSETR('OX',0.5)
.br
CALL NGSETR('OY',0.5)
.br
CALL NGSETR('OS',0.1)
.br
CALL NGSETI('LC',2)
.br
CALL NGSETI('LB',3)
.br
CALL NGEZLOGO()
.sp
Plots a logo of type 5 (UCAR star with text "UCAR") in the middle
of the plot with size one-tenth the maximum plot height using color
index 2 for the star and color index 3 for the text.
.sp
.RE
.SH ACCESS
To use NGEZLOGO or c_ngezlogo, load the NCAR Graphics 
libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
nglogo, ngseti, ngsetr (3NCARG),
.sp
Hardcopy:
NCAR Graphics Fundamentals, UNIX Version
.SH COPYRIGHT
Copyright (C) 1987-2002
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
