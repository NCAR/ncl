.TH Ngmisc 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Ngmisc - A collection of miscellaneous NCAR Graphics routines.
.SH SYNOPSIS
NGDOTS - Draws filled circular dots at coordinate positions
(X(I),Y(I),I=1,NUM) at size SIZE with color given by the color index ICOLOR.
.sp
NGGCOG - Returns the latitudes and longitudes of a set of points approximating
a circle at a given point on the surface of the globe.
.sp
NGGSOG - Returns the latitudes and longitudes of six points defining a
five-pointed star at a given point on the surface of the globe.
.sp
NGPICT - Effects a break in the picture drawing sequence in a FORTRAN
code using GKS.  The actions taken depend on whether the designated
workstation is a metafile or an output/input workstation.  An option
is provided for prompting the user when an output/input workstation is
ready and waiting after a pause.
.sp
NGRITD - Given the coordinates of a point, this routine performs a rotation
of that point about a specified axis by a specified angle.
.sp
NGWSYM - Draws a symbol from the standard WMO/NOAA meteorological
fonts by reference to the font name and symbol number within that
font.
.SH C-BINDING SYNOPSIS
c_ngdots
.br
c_nggcog
.br
c_nggsog
.br
c_ngpict
.br
c_ngritd
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
NGGCOG(3NCARG)
NGGSOG(3NCARG)
NGPICT(3NCARG),
NGRITD(3NCARG)
NGWSYM(3NCARG)
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
