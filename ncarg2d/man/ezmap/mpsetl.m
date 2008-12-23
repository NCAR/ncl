.TH MAPSTL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPSTL - 
Sets the values of certain EZMAP parameters of type LOGICAL.
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
EZMAP parameters.  For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
The capabilities offered by the MAPSTL or MPSETL routines are
also offered by the MAPSTI or MPSETI routines. See the examples
listed on the MAPSTI or MPSETI man page.
.SH ACCESS
To use MAPSTL, MPSETL, or c_mapstl, load the NCAR Graphics libraries ncarg, 
ncarg_gks, and ncarg_c, preferably in that order.
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
Copyright (C) 1987-2009
.br
University Corporation for Atmospheric Research
.br
The use of this Software is governed by a License Agreement.
