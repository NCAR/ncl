.TH MAPGRD 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPGRD -
Draws a lat/lon grid. 
.SH SYNOPSIS
CALL MAPGRD 
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapgrd()
.SH DESCRIPTION
MAPGRD has no arguments.
.SH USAGE
The statement:
.RS 5
.sp
CALL MAPGRD
.RE
.sp
draws a grid consisting of lines of constant latitude (parallels) and lines
of constant longitude (meridians), spaced as specified by the value of the
internal parameter 'GR'.  If EZMAP needs initialization or if the value
of 'GR' is less than or equal to zero or if there is an uncleared NCAR Graphics
error, MAPGRD does nothing.
.sp
The grid is drawn using calls to MAPIT and MAPIQ.  By default, MAPGRD
temporarily forces the value of the internal parameter 'DL' to zero, so
that the grid will be drawn using calls to the routines FRSTD and VECTD,
in the dash package.  MAPGRD also temporarily forces the use of the dash
pattern defined by the internal parameter 'DA'.  A user version of the
routine MAPUSR may be supplied to change the way in which grid lines
are drawn.  Before returning control to the user, MAPGRD restores the
original value of 'DL' and resets the current dash pattern to be used
by the dash package.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: 
ccpcica,
ccpmovi,
cmpclr,
cmpdd,
cmpgrd,
cmplbl,
cmplot,
cmptit,
epltch,
ffex02,
fpchiqu.
.SH ACCESS
To use MAPGRD or c_mapgrd, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
