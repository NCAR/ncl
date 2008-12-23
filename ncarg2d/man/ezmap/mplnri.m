.TH MPLNRI 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPLNRI - Reads information from a specified EZMAP database.
.SH SYNOPSIS
CALL MPLNRI (FLNM)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mplnri (char *flnm)
.SH DESCRIPTION 
.IP FLNM 12
(an input expression of type CHARACTER) specifies the name of the database to
be used.  MPLNRI will first look for the files of the specified database in
the current working directory; if the files are not found there, MPLNRI will
look for them in the NCAR Graphics database directory.  The database created in
1998 and provided as part of Version 4.1 of NCAR Graphics is named "Earth..1".
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
Calling MPLNRI causes information about the database whose name is specified
by FLNM to be read into EZMAPB common blocks.  Subsequent references to
functions like MPIOAR, MPISCI, and so on make use of this data.
.SH EXAMPLES
Use the ncargex command to see the following relevant example: mpex12.
.SH ACCESS
To use MPLNRI or c_mplnri, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
