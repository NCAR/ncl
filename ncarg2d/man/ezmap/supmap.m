.TH SUPMAP 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
Draws a map with a single call. An implementation of the routine
from which EZMAP grew.
.SH SYNOPSIS
 CALL SUPMAP (JPRJ, PLAT, PLON, ROTA, PLM1, PLM2, PLM3, 
.br
+ PLM4, JLTS, JGRD, IOUT, IDOT, IERR)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_supmap (int jprj, float plat, float plon, 
.br
float rota, float *plm1, float *plm2, float *plm3, 
.br
float *plm4, int jlts, int jgrd, int iout, int idot, 
.br
int *ierr)
.SH DESCRIPTION 
.IP JPRJ 12
(input expression, of type INTEGER) defines the projection type and
indicates whether or not continental outlines are to be plotted, as
follows:
.RS 
.IP \(bu 4 
IABS(JPRJ) defines the projection type, as follows (values less than 1 or
greater than 10 are treated as 1 or 10, respectively):
.RS 8 
.IP IABS(JPRJ) 12
Projection Type
.IP "1" 12
Stereographic.
.IP "2" 12
Orthographic.
.IP "3" 12
Lambert conformal
conic.
.IP "4" 12
Lambert equal area.
.IP "5" 12
Gnomonic.
.IP "6" 12
Azimuthal equidistant.
.IP "7" 12
Satellite view.
.IP "8" 12
Cylindrical
equidistant.
.IP "9" 12
Mercator.
.IP "10" 12
Mollweide-type.
.RE
.IP "" 4
Using the value 2 causes the EZMAP parameter 'SA' to be zeroed. ('SA', if
greater than 1., says that a satellite-view projection, rather than an
orthographic projection, is to be used, and specifies the distance of the
satellite from the center of the earth, in units of earth radii.)
.sp
Using the value 7 causes 'SA' to be examined. If it has a non-zero value,
the value is left alone. If it has a zero value, its value is reset to
6.631, which is about right for a satellite in a geosynchronous
equatorial orbit.
.RE
.RS
.IP \(bu 4
The sign of JPRJ, when IOUT is -1, 0, or +1, indicates whether the
continental outlines are to be plotted or not. See IOUT, below.
.RE
.IP "PLAT, PLON, and ROTA" 12 
(input expressions, of type REAL) define the origin
of the projection and its rotation angle and are used in the same way as
they would be in a call to the routine MAPROJ (which see).
.IP JLTS 12
(input expression, of type INTEGER), and PLM1, PLM2, PLM3, and PLM4
(input arrays, dimensioned 2, of type REAL) specify the rectangular
limits of the map. These arguments are used in the same way as they would
be in a call to MAPSET (which see), except that JLTS is an integer
instead of a character string. IABS(JLTS) may take on the values 1
through 5, as follows:
.RS 16
.IP IABS(JLTS) 12
Equivalent character string in a call to
MAPSET
.IP "1" 12
\&'MA' - maximal area desired.
.IP "2" 12
\&'CO' - corner points given in PLM1, PLM2,
PLM3, and PLM4.
.IP "3" 12
\&'LI' - U/V limits given in PLM1, PLM2, PLM3,
and PLM4.
.IP "4" 12
\&'AN' - angles given in PLM1, PLM2, PLM3, and
PLM4.
.IP "5" 12
\&'PO' - edge points given in PLM1, PLM2, PLM3,
and PLM4.
.RE
.IP "" 12
At one time, the sign of JLTS specified whether or not a line of text was
to be written at the bottom of the plot produced. This line may no longer
be written and the sign of JLTS is therefore ignored.
.IP JGRD 12
(input expression, of type INTEGER) is used in the following way:
The value of "MOD(IABS(JGRD),1000)" is the value, in degrees, of the
interval at which lines of latitude and longitude are to be plotted. If
the given interval is zero, grid lines and labels are not plotted. If
JGRD is less than zero, the perimeter is not plotted. Set JGRD to -1000
to suppress both grid lines and perimeter and to +1000 to suppress the
grid lines, but leave the perimeter. The value -0 may have a meaning on
ones' complement machines, but should be avoided; use -1000 instead.
.IP IOUT 12
(input expression, of type INTEGER) has the value 0 to suppress U.S.
state outlines, and the value -1 or +1 to plot U.S. state outlines. In
both of these cases, the sign of JPRJ indicates whether continental
outlines are to be plotted (JPRJ positive) or not (JPRJ negative).
Originally, SUPMAP recognized only these values of IOUT; now, if IOUT is
less than 0 or greater than 1, the sign of JPRJ is ignored, and IOUT
selects an outline group, as follows:
.RS 16
.IP IOUT 12
Outline group selected
.IP "-2 or less" 12
\&'NO' (no outlines)
.IP "2" 12
\&'CO' (continental outlines)
.IP "3" 12
\&'US' (U.S. state outlines)
.IP "4" 12
\&'PS' (continental outlines plus international outlines
plus U.S. state outlines)
.IP "5 or greater" 12
\&'PO' (continental outlines plus international
outlines)
.RE
.IP "" 12
At one time, the sign of IOUT specified whether or not a line of text was
to be written on the print output unit. This may no longer be done.
.IP IDOT 12
(input expression, of type INTEGER) is a 0 to get continuous
outlines, a 1 to get dotted outlines.
.IP IERR 12
(output variable, of type INTEGER) is the only output parameter. A
non-zero value indicates that an error has occurred. The section "ERROR
CONDITIONS" lists the possible values of IERR.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
caredg,
cmpgci,
cmpsup,
mpex03,
mpex08,
mpexfi,
bnchmk,
stex03,
vvex02.
.SH ACCESS
To use SUPMAP or c_supmap, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.
.SH SEE ALSO
Online:
ezmap,
ezmap_params,
mapaci,
mapbla,
mapblm,
mapdrw,
mapeod,
mapfst,
mapgci,
mapgrd,
mapgrm,
mapgtc,
mapgti,
mapgtl,
mapgtr,
mapint,
mapiq,
mapiqa,
mapiqd,
mapiqm,
mapit,
mapita,
mapitd,
mapitm,
maplbl,
maplmb,
maplot,
mappos,
maproj,
maprs,
maprst,
mapsav,
mapset,
mapstc,
mapsti,
mapstl,
mapstr,
maptra,
maptri,
maptrn,
mapusr,
mapvec,
mpchln,
mpfnme,
mpgetc,
mpgeti,
mpgetl,
mpgetr,
mpglty,
mpiaty,
mpifnb,
mpilnb,
mpiola,
mpiosa,
mpipai,
mpipan,
mpipar,
mpisci,
mplnam,
mplndm,
mplndr,
mplnri,
mpname,
mprset,
mpsetc,
mpseti,
mpsetl,
mpsetr,
supcon,
ncarg_cbind
.sp
Hardcopy:  
NCAR Graphics Contouring and Mapping Tutorial 
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
