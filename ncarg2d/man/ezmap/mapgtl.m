.TH MAPGTL 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPGTL - 
Gets the current value of a specified Ezmap 
parameter of type LOGICAL. 
.sp
MPGETL is an alternate name for the routine MAPGTL.
.SH SYNOPSIS
CALL MAPGTL (PNAM,LVAL)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
void c_mapgtl (char *pnam, int *lval)
.SH DESCRIPTION 
.IP PNAM 12
(an input expression of type CHARACTER) specifies the name of the
parameter to get. Only the first two characters of the string are
examined. 
.IP LVAL 12
(an output variable of type LOGICAL) 
receives the value of the parameter
specified by PNAM.
.SH C-BINDING DESCRIPTION
The C-binding argument descriptions are the same as the FORTRAN 
argument descriptions.
.SH USAGE
This routine allows you to retrieve the current value of
Ezmap parameters.  For a complete list of parameters available
in this utility, see the ezmap_params man page.
.SH EXAMPLES
The capabilities offered by the MAPGTL or MPGETL routines are
also offered by the MAPGTI or MPGETI routines. See the examples
listed on the MAPGTI or MPGETI man page.
.SH ACCESS
To use MAPGTL or MPGETL, load the NCAR Graphics libraries ncarg, 
ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.  To use 
c_mapgtl or c_mpgetl, load the NCAR Graphics libraries ncargC, 
ncarg_gksC, ncarg, ncarg_gks, ncarg_c, and ncarg_loc, preferably in that order.
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
.SH COPYRIGHT
Copyright 1987, 1988, 1989, 1991, 1993 University Corporation
for Atmospheric Research
.br
All Rights Reserved
