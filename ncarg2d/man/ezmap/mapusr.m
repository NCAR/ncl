.TH MAPUSR 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPUSR - Called by EZMAP routines that draw the various parts of the map.
The default version does nothing.  A user-written version may be supplied
to change the appearance of the map. Note that this routine is not called
by any of the EZMAPB routines; they call MPCHLN instead.
.SH SYNOPSIS
CALL MAPUSR (IPRT)
.SH DESCRIPTION 
.IP IPRT 12
(input expression, of type INTEGER), if positive, says that a
particular part of the map is about to be drawn, as follows:
.RS 16 
.IP IPRT 6
Part of map about to be drawn
.IP "1" 6
Perimeter.
.IP "2" 6 
Grid.
.IP "3" 6 
Labels.
.IP "4" 6
Limb lines.
.IP "5" 6
Continental outlines.
.IP "6" 6
U.S. state outlines.
.IP "7" 6
International outlines.
.RE
.IP "" 12
If IPRT is negative, then the drawing of the last
part is complete. The absolute value of IPRT will be
one of the above values. Changed quantities should be
restored.
.SH USAGE
EZMAP executes the statement
.sp
.RS 4
CALL MAPUSR (IPRT)
.RE
.sp
just before and just after each portion of a map is drawn. The default
version of MAPUSR does nothing.
.sp
A user-supplied version of MAPUSR may set/reset the dotting parameter
\&'DL', the DASHCHAR dash pattern, the color index, etc., so as to achieve
a desired effect.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: 
mpex08.
.SH ACCESS
To use MAPUSR, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
.SH COPYRIGHT
Copyright (C) 1987-2000
.br
University Corporation for Atmospheric Research
.br

This documentation is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA.
