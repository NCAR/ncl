.TH MAPSAV 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPSAV - Saves the current state of EZMAP for later restoration
by MAPRST.
.SH SYNOPSIS
CALL MAPSAV (INFO)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapsav (int info)
.SH DESCRIPTION 
.IP INFO 12
(an input expression, of type INTEGER) is the number of a unit to
which a single unformatted record is to be written. It is the user's
responsibility to position this unit. MAPSAV does not rewind it, either
before or after writing the record.
.SH C-BINDING DESCRIPTION 
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
This routine allows you to save internal parameter values to a file. 
For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
No example is available for MAPSAV.
.SH ACCESS
To use MAPSAV or c_mapsav, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
maproj,
maprs,
maprst,
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
