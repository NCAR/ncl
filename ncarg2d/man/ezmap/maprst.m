.TH MAPRST 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPRST - Restores the state of Ezmap saved by an earlier call to MAPSAV.
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
To use MAPRST, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  To use c_maprst, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
and ncarg_c, preferably in that order.
.SH MESSAGES
See the ezmap man page for a description of all Ezmap error
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
mpgetc,
mpgeti,
mpgetl,
mpgetr,
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
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
