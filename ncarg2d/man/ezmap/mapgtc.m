.TH MAPGTC 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPGTC - 
Gets the current value of a specified EZMAP parameter of type CHARACTER.
.sp
MPGETC is an alternate name for the routine MAPGTC.
.SH SYNOPSIS
CALL MAPGTC (PNAM,CVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapgtc (char *pnam, char *cval, int len)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression of type CHARACTER) specifies the name of the
parameter to get. Only the first two characters of the string are
examined. 
.IP CVAL 12
(an output variable of type CHARACTER) 
receives the value of the parameter
specified by PNAM.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN
argument descriptions with the following exception:
.IP len 12
The size of cval as dimensioned in the calling program.
.SH USAGE
This routine allows you to retrieve the current value of
EZMAP parameters.  For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: mpex07.
.SH ACCESS
To use MAPGTC, MPGETC, or c_mapgtc load the NCAR Graphics libraries ncarg, 
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
