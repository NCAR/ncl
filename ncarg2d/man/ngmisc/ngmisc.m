.TH NGMISC 3NCARG "April 1994" UNIX "NCAR GRAPHICS"
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
NGGETC - Retrieves values for character-valued parameters set by NGSETC.
.sp
NGGETI - Retrieves values for integer-valued parameters set by NGSETI.
.sp
NGGETR - Retrieves values for real-valued parameters set by NGSETR.
.sp
NGSETC - Sets values for character-valued parameters.
.sp
NGSETI - Sets values for integer-valued parameters.
.sp
NGSETR - Sets values for real-valued parameters.
.sp
NGGSOG - Returns the latitudes and longitudes of six points defining a
five-pointed star at a given point on the surface of the globe.
.sp
NGPICT - Effects a break in the picture drawing sequence in a FORTRAN
code using NCAR GKS.  The actions taken depend on whether the designated
workstation is a metafile or an output/input workstation.  An option
is provided for prompting the user when an output/input workstation is
ready and waiting after a pause.
.sp
NGPSWK - Returns an integer workstation type for NCAR GKS PostScript 
workstation types.
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
c_nggetc
.br
c_nggeti
.br
c_nggetr
.br
c_nggcog
.br
c_nggsog
.br
c_ngpict
.br
c_ngpswk
.br
c_ngritd
.br
c_ngsetc
.br
c_ngseti
.br
c_ngsetr
.br
c_ngwsym
.SH USER-MODIFIABLE INTERNAL ROUTINES
None.
.SH ACCESS 
To use Ngmisc routines, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
NGDOTS(3NCARG),
NGSETC(3NCARG),
NGSETI(3NCARG),
NGSETR(3NCARG),
NGGCOG(3NCARG),
NGGETC(3NCARG),
NGGETI(3NCARG),
NGGETR(3NCARG),
NGGSOG(3NCARG),
NGMISC_PARAMS(3NCARG),
NGPICT(3NCARG),
NGPSWK(3NCARG),
NGRITD(3NCARG),
NGWSYM(3NCARG)
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
