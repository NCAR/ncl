.TH MPLNAM 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPLNAM - Reads a specified EZMAP database and sends boundary lines from it to a
specified area map.
.SH SYNOPSIS
CALL MPLNAM (FLNM,ILVL,IAMA)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mplnam (char *flnm, int ilvl, int *iama)
.SH DESCRIPTION 
.IP FLNM 12
(an input expression of type CHARACTER) specifies the name of the database to
be used.  MPLNAM will first look for the files of the specified database in
the current working directory; if the files are not found there, MPLNAM will
look for them in the NCAR Graphics database directory.  The database created in
1998 and provided as part of Version 4.1 of NCAR Graphics is named "Earth..1".
.IP ILVL 12
(an input expression of type INTEGER) specifies the level at which the database
is to be used.  The value 1 says to use only land/water boundaries, the value 2
says to add continental boundaries (like the boundary which separates Africa
from Eurasia), the value 3 says to add the boundaries of countries, and
the value 4 says to add states.  (The value 5 will eventually be used to add
counties.)
.IP IAMA 12
(an input/output array of type INTEGER) is the area map array to which boundary
lines are to be added.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
Calling MPLNAM adds boundary lines to the area map in the array IAMA. The area
map must previously have been initialized by calling the routine ARINAM, in the
package AREAS, using a statement like "CALL ARINAM (IAMA,LAMA)", where LAMA is
the length of the array IAMA. The area map may subsequently be used in various
ways; for example, one may call the AREAS routine ARSCAM to draw a solid-filled
map.
.sp
One or two groups of boundary lines are added to the area map by a call to
MPLNAM. The first, having group identifier 'G1' (default value 1), consists
of a perimeter (either rectangular or elliptical, depending on the value of
the internal parameter 'EL') and the set of projected boundary lines implied
by the user's selection of a map database and level.  For certain projections,
a limb line may also be included.
.sp
If the parameter 'VS' has a value greater than zero, the group 'G2' is added
to the area map; it consists of a copy of the perimeter and the limb line (if
any) plus a set of vertical lines splitting the area inside the perimeter
into 'VS' vertical strips. (By default, the value of 'VS' is 1.) The object
of the group 'G2' is to split areas up, reducing the number of points required
to define a typical area below the level at which some target hardware device
begins to fail.  (This is more important when using MPLNAM than it was when
using MAPBLA because the new database is at a higher resolution.)
.sp
The perimeter and the limb in the groups 'G1' and 'G2' have the following left
and right area identifiers:
.RS 4
.IP 0 4 
Identifies the area inside the perimeter or limb.
.IP -1 4 
Identifies the area outside the perimeter or limb.
.RE
.sp
The vertical lines in the group 'G2' have left and right area identifiers of 0.
.sp
To set the values of 'G1', 'G2', and 'VS', call the EZMAP routine MAPSTI.  To
get the current values of 'G1', 'G2', and \&'VS', call the EZMAP routine
MAPGTI.  See the man pages for MAPSTI and MAPGTI.
.SH EXAMPLES
Use the ncargex command to see the following relevant examples: mpex11, tezmpb.
.SH ACCESS
To use MPLNAM or c_mplnam, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
