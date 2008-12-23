.TH MPIFNB 3NCARG "April 1998" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MPIFNB - A simple utility function that finds the index of the first non-blank
character in a character string.
.SH SYNOPSIS
IFNB=MPIFNB(CHRS)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_mpifnb (char *chrs)
.SH DESCRIPTION 
.IP CHRS 12
is an input expression of type CHARACTER.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
The statement "IFNB=MPIFNB(CHRS)" sets the value of IFNB to the index (between
1 and LEN(CHRS)) of the first non-blank character in the character string CHRS.
.SH EXAMPLES
None.
.SH ACCESS
To use MPIFNB or c_mpifnb, load the NCAR Graphics libraries ncarg, ncarg_gks,
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
