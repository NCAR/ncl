.TH MAPSAV 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPSAV - Saves the current state of Ezmap for later restoration
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
To use MAPSAV, load the NCAR Graphics libraries ncarg, ncarg_gks,
ncarg_c, and ncarg_loc, preferably in that order.  To use c_mapsav, load 
the NCAR Graphics libraries ncargC, ncarg_gksC, ncarg, ncarg_gks, 
ncarg_c, and ncarg_loc, preferably in that order.
.SH MESSAGES
See the ezmap man page for a description of all Ezmap error
messages and/or informational messages.
.SH SEE ALSO
Online:
ezmap,
ezmap_params,
mapaci,
mapbla,
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
Tutorial: A Step-by-Step Guide to Contouring and Mapping
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
