.TH MAPROJ 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh 
.SH NAME
MAPROJ - Sets the map projection to be used.
.SH SYNOPSIS
CALL MAPROJ (JPRJ, PLAT, PLON, ROTA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maproj (char *jprj, float plat, float plon, 
.br
float rota)
.SH DESCRIPTION 
.IP JPRJ 12
(an input expression, of type CHARACTER) defines the desired
projection type. All the possible values are two characters in length;
these are the possibilities:
.sp
.RS 12
The conic projection:
.RS 4
.IP "'LC' - " 8
Lambert conformal conic with two standard
parallels.
.RE
.sp
The azimuthal projections:
.RS 4
.IP "'ST' - " 8
Stereographic.
.IP "'OR' - " 8
Orthographic. The EZMAP parameter 'SA' will be
zeroed. See the note below.
.IP "'LE' - " 8
Lambert equal area.
.IP "'GN' - " 8
Gnomonic.
.IP "'AE' - " 8
Azimuthal equidistant.
.IP "'SV' - " 8 
Satellite-view. If the EZMAP parameter 'SA' is
less than or equal to 1., it will be reset to
6.631 (the value for a satellite in a
geosynchronous orbit).  See the note below.
.RE
.sp
The cylindrical projections:
.RS 4
.IP "'CE' - " 8
Cylindrical equidistant.
.IP "'ME' - " 8
Mercator.
.IP "'MO' - " 8
Mollweide-type. 
.RE
.sp
Note: The orthographic and satellite-view projections
have the same internal identifier. The EZMAP
parameter 'SA' determines which will be used. If a call
to MAPROJ selecting one or the other is followed by a
call to MAPSTR resetting 'SA', it may have the effect
of causing the other to be used.  
.RE
.IP "PLAT,PLON,ROTA" 12
(input expressions, of type REAL) are angular
quantities, in degrees. How they are used depends on the value of JPRJ,
as follows:
.RS
.IP \(bu 4
If JPRJ is not equal to 'LC': PLAT and PLON define
the latitude and longitude of the pole of the projection,
the point on the globe which is to project to
the origin of the u/v plane. PLAT must be between -90.
and +90., inclusive, positive in the northern
hemisphere, negative in the southern. PLON must be
between -180. and +180., inclusive, positive to the
east, and negative to the west, of Greenwich.  ROTA is
the angle between the v axis and north at the origin.
It is taken to be positive if the angular movement from
north to the v axis is counter-clockwise, negative
otherwise. If the origin is at the north pole, "north"
is considered to be in the direction of PLON+180.  If
the origin is at the south pole, "north" is considered
to be in the direction of PLON. For the cylindrical
projections, the axis of the projection is parallel to
the v axis.
.IP \(bu 4
If JPRJ is equal to 'LC': PLON defines the central
meridian of the projection, and PLAT and ROTA define
the two standard parallels. If PLAT and ROTA are equal,
a simpler conic projection, with one standard parallel,
is used.  
.RE
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the current values of the
EZMAP parameters 'PR', 'PT', 'PN', and 'RO'.
For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpcica,
ccpmap,
ccpmovi,
ccppole,
ccpvp,
colcon,
cezmap1,
cezmap2,
cezmap3,
cmpclr,
cmpdd,
cmpdrw,
cmpel,
cmpfil,
cmpgrd,
cmpgrp,
cmpita,
cmpitm,
cmplab,
cmplbl,
cmplot,
cmpmsk,
cmpou,
cmpsat,
cmptit,
cmptra,
cmpusr,
cpex01,
cpex03,
cpex08,
cpex09,
mpex01,
mpex02,
mpex04,
mpex05,
mpex06,
mpex07,
mpex09,
mpex10,
eezmpa,
epltch,
tezmap,
tezmpa,
fcover,
ffex00,
ffex02,
ffex03,
ffex05,
fgkgtx,
fngngdts,
fpchiqu.
.SH ACCESS
To use MAPROJ or c_maproj, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH MESSAGES
See the ezmap man page for a description of all EZMAP error
messages and/or informational messages.
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
supmap,
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
