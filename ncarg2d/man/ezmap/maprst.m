.TH MAPRST 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPRST - Restores the state of EZMAP saved by an earlier call to MAPSAV.
.SH SYNOPSIS
CALL MAPRST (INFO)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_maprst (int info)
.SH DESCRIPTION 
.IP INFO 12
(an input expression, of type INTEGER) is the number of a unit from
which a single unformatted record is to be read. It is the user's
responsibility to position this unit. MAPRST does not rewind it, either
before or after reading the record.
.SH C-BINDING DESCRIPTION 
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
This routine allows you to restore internal parameter values
from a file. 
For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
No example is available for MAPRST.
.SH ACCESS
To use MAPRST or c_maprst, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
