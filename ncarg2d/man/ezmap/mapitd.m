.TH MAPITD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPITD - Draws lines on a map.  MAPITD is just like MAPIT, but, ultimately,
DASHPACK routines are called instead of DASHCHAR routines.
.SH SYNOPSIS
CALL MAPITD (RLAT,RLON,IFST)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapitd (float rlat, float rlon, int ifst)
.SH DESCRIPTION 
.IP "RLAT and RLON" 12 
(input expressions, of type REAL) specify the latitude and
longitude of a point to which the "pen" is to be moved. Both are given in
degrees. RLAT must be between -90. and +90., inclusive; RLON must be
between -540. and +540., inclusive.
.IP IFST 12 
(an input expression, of type INTEGER) is 0 to do a "pen-up" move, 1
to do a "pen-down" move only if the distance from the last point to the
new point is greater than 'MV' plotter units, and 2 or greater to do a
"pen-down" move regardless of the distance from the last point to the new
one.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
MAPITD is used to draw lines on the map.  MAPITD attempts to omit
nonvisible portions of lines and to handle "crossover," a
jump from one end of the map to the other caused by the
projection of the globe onto a flat surface. Crossover can 
occur on cylindrical and conical projections; MAPITD handles
it gracefully on the former and not so well on the latter. 
.sp
To draw the projection of a line defined by a series of lat/lon
coordinates, start by calling MAPITD with the coordinates of the
first point and with IFST = 0.  Then, call MAPITD repeatedly with
the coordinates of the next point along the line and with IFST = 1
or 2.
(IFST = 2 is normally used only for the final point, to
ensure closure with a line drawn by another series of calls.)
Finally, if the next thing your program does is STOP, call FRAME,
or change attributes like color or line width, you should call MAPIQD
to flush MAPITD's buffers.
.sp
The EZMAP parameter 'DL' determines whether MAPITD draws solid
lines or dotted lines. Dotted lines are drawn using calls to
POINTS. Solid lines are drawn using calls to DPFRST and
DPVECT. The parameters 'DD' and 'MV' also affect MAPITD's
behavior. See the descriptions of these parameters in the
ezmap_params man page.
.sp
Keep in mind the following:
.IP \(bu 4
The projection of the line segment joining two points on the
globe is considered to be the straight-line segment joining the
projections of the points; no attempt is made to project it as
if it were a portion of a great circle.
.IP \(bu 4
If both endpoints of a line segment are visible, the segment
is considered to be entirely visible.
.IP \(bu 4
If both endpoints are invisible, the segment is considered
to be entirely invisible.
.IP \(bu 4
If one endpoint is visible and the other is not, a new point
is interpolated at the boundary between the visible and
invisible portions.
Only visible portions of the line are drawn.
.LP
Because of these considerations, points defining a line should not be
too far apart on the globe.
.sp
There are two types of boundaries between visible and invisible regions:
.IP \(bu 4
The limb is a boundary between a projectable region and an
unprojectable one. The limb may be circular, elliptical, or
some other shape, depending on the projection being used. For
example, an orthographic projection has as its limb a circle,
centered at (0,0), with a radius of 1.
.IP \(bu 4
The perimeter is a rectangular or elliptical boundary
defined by EZMAP parameters set by you to specify the region
you wish to view.
.SH EXAMPLES
None.
.SH ACCESS
To use MAPITD or c_mapitd, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
