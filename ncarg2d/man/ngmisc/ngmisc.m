.TH Ngmisc 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Ngmisc - A collection of miscellaneous NCAR Graphics routines.
.SH SYNOPSIS
NGDOTS - Draws filled circular dots at coordinate positions
(X(I),Y(I),I=1,NUM) at size SIZE with color given by the color index ICOLOR.
.sp
NGPICT - Effects a break in the picture drawing sequence in a FORTRAN
code using GKS.  The actions taken depend on whether the designated
workstation is a metafile or an output/input workstation.  An option
is provided for prompting the user when an output/input workstation is
ready and waiting after a pause.
.sp
NGWSYM - Draws a symbol from the standard WMO/NOAA meteorological
fonts by reference to the font name and symbol number within that
font.
.SH C-BINDING SYNOPSIS
c_ngdots
.br
c_ngpict
.br
c_ngwsym
.SH USER-MODIFIABLE INTERNAL ROUTINES
None.
.SH ACCESS 
To use Ngmisc routines, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.  To use the C bindings, load the 
NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks,
and ncarg_loc, preferably in that order.
.SH SEE ALSO
Online:
NGDOTS(3NCARG),
NGPICT(3NCARG),
NGWSYM(3NCARG)
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
