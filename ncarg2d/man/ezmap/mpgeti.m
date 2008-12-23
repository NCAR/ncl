.TH MAPGTI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPGTI - 
Gets the current value of a specified EZMAP
parameter of type INTEGER. 
.sp
MPGETI is an alternate name for the routine MAPGTI.     
.SH SYNOPSIS
CALL MAPGTI (PNAM,IVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapgti (char *pnam, int *ival)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression of type CHARACTER) specifies the name of the
parameter to get. Only the first two characters of the string are
examined. 
.IP IVAL 12
(an output variable of type INTEGER)
receives the value of the parameter
specified by PNAM.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
EZMAP parameters.  For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
Use the ncargex command to see the following relevant
examples: mpex07, mpex08.
.SH ACCESS
To use MAPGTI, MPGETI, or c_mapgti load the NCAR Graphics libraries ncarg, 
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
