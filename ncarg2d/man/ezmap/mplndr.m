.TH MPLNDR 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPLNDR - Reads a specified EZMAP database and draws boundary lines from it.
.SH SYNOPSIS
CALL MPLNDR (FLNM,ILVL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mplndr (char *flnm, int ilvl)
.SH DESCRIPTION 
.IP FLNM 12
(an input expression of type CHARACTER) specifies the name of the database to
be used.  MPLNDR will first look for the files of the specified database in
the current working directory; if the files are not found there, MPLNDR will
look for them in the NCAR Graphics database directory.  The database created in
1998 and provided as part of Version 4.1 of NCAR Graphics is named "Earth..1".
.IP ILVL 12
(an input expression of type INTEGER) specifies the level at which the database
is to be used.  The value 1 says to use only land/water boundaries, the value 2
says to add continental boundaries (like the boundary which separates Africa
from Eurasia), the value 3 says to add the boundaries of countries, and the
value 4 says to add states.  (The value 5 will eventually be used to add
counties.)
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
MPLNDR draws the lines defined by the map database whose name is FLNM; the
EZMAP internal parameter 'DO' determines whether solid lines or dotted lines
are used.  If EZMAP currently needs initialization, MPLNDR does nothing except
read some information from the map database, so that subsequent calls to EZMAPB
functions will work properly.
.sp
The outlines are drawn using calls to MAPIT and MAPIQ.  By default, MPLNDR
forces the value of the internal parameter 'DL' equal to the value of the
internal parameter 'DO'; it also supplies the dash package with a solid-line
dash pattern.  When 'DO' is zero, the outlines are drawn using calls to the
routines FRSTD and VECTD, in the dash package, and this gives solid lines.
When 'DO' is non-zero, the outlines are drawn using calls to the SPPS routine
POINTS, which gives dotted lines.  Before returning control to the user, MAPLOT
restores the original value of 'DL' and the original dash pattern.  A user
version of the routine MPCHLN may be supplied to change the way in which the
outlines are drawn.
.SH EXAMPLES
Use the ncargex command to see the following relevant example: tezmpb.
.SH ACCESS
To use MPLNDR or c_mplndr, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
.SH COPYRIGHT
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
