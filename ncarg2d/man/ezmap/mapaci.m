.TH MAPACI 3NCARG "March 1993" UNIX "NCAR GRAPHICS"
.na
.nh
.SH NAME
MAPACI - 
Normally used to pick colors for the areas created by the boundary lines added
to an area map by a call to MAPBLA.  Note that this function should not be used
to select color indices for areas defined by the new map database "Earth..1",
which was created in 1998; for that purpose, use EZMAPB functions instead (in
particular, MPISCI).
.SH SYNOPSIS
ICIR=MAPACI(IAID)
.SH C-BINDING SYNOPSIS
#include <ncarg/ncargC.h>
.sp
int c_mapaci (int iaid)
.SH DESCRIPTION 
.IP IAID 12 
(an input expression of type INTEGER) is an area identifier for one
of the areas defined by the set of boundary lines added to an area map by
a call to MAPBLA.
.SH C-BINDING DESCRIPTION
The C-binding argument description is the same as the FORTRAN 
argument description.
.SH USAGE
Executing the statement "ICIR=MAPACI(IAID)" gives ICIR a value between 1 and 7,
inclusive, that may be used to select a color index for the area whose area
identifier is IAID.  It is guaranteed that MAPACI will not return the same
value for adjacent areas; this fact can be used to ensure that adjacent areas
will have different colors.
.sp
Note that it is the responsibility of the user to define the colors
associated with color indices
used; this is done by calling the GKS routine GSCR.
.SH EXAMPLES
Use the ncargex command to see the following relevant
example: eezmpa.
.SH ACCESS
To use MAPACI or c_mapaci, load the NCAR Graphics libraries ncarg, ncarg_gks,
and ncarg_c, preferably in that order.  
.SH SEE ALSO
Online:
ezmap, 
ezmap_params, 
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
