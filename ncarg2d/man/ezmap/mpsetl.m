.TH MAPSTL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPSTL - 
Sets the values of certain Ezmap 
parameters of type LOGICAL. 
.sp
MPSETL is an alternate name for the routine MAPSTL.
.SH SYNOPSIS
CALL MAPSTL (PNAM,LVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapstl (char *pnam, int lval)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression, of type CHARACTER*2) specifies the name of an
internal parameter whose value is to be set. Only the first two
characters of this string are examined.
.IP LVAL 12
(an input expression of type LOGICAL) 
contains the value to be given to
the parameter specified by PNAM.
.SH C-BINDING DESCRIPTION 
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to set the value of
Ezmap parameters.  For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
The capabilities offered by the MAPSTL or MPSETL routines are
also offered by the MAPSTI or MPSETI routines. See the examples
listed on the MAPSTI or MPSETI man page.
.SH ACCESS
To use MAPSTL or MPSETL, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_loc, preferably in that order.  To use 
c_mapstl or c_mpsetl, load the NCAR Graphics libraries ncargC, 
ncarg_gksC, ncarg, ncarg_gks, and ncarg_loc, preferably in that order.
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
mapsav,
mapset,
mapstc,
mapsti,
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
